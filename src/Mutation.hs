{-|
Module      : ModelConversions
Description : Conversions between models of Julia PL's types
License     : GPL-3
Maintainer  : a.pelenitsyn@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Mutation (Mut, mutate) where

import Generics.Pointless.Combinators       (split)
import Control.Monad.Trans.Class            (lift)
import Control.Monad.Trans.State            (StateT(..), liftListen)
import Control.Monad                        (join)
import qualified Data.Set as Set
import Math.Combinatorics.CombinatorialHopfAlgebra (deconcatenations)

import qualified QModel as Q
import qualified SModel as S

-- **************
-- Types go first
-- **************

type Vars = Set.Set S.Var

-- | Conversion monad stores
--  1) how many fresh variables we have used during the conversion
--  2) the names of the free variables in the current tree
type Mut a = StateT Int [] (a, Vars) 

-- *******************************************
-- Conversion Tools (go from QModel to SModel)
-- *******************************************

-- | After conversion we should get a plain list of types
exitMut :: Mut a -> [a]
exitMut = map fst 
           . filter (Set.null . snd) 
           . map fst 
           . flip runStateT 0

mutator :: Q.QTau -> Mut S.Tau
mutator = m where
    m Q.H          = mutHole
    m (Q.A q)      = mutApp q
    m (Q.P q1 q2)  = mutBin S.P q1 q2
    m (Q.U q1 q2)  = mutBin S.U q1 q2

mutate ::Q.QTau -> [S.Tau]
mutate = exitMut
          . (>>= lift . uncurry combineTrivAugmenter)
          . mutator

-- ****************************
-- Auxiliary conversion actions
-- ****************************

mutHole :: Mut S.Tau
mutHole = StateT $ \n ->
              zip (zip knownTypes $ repeat Set.empty)
                   [0, 0..] 
           ++ zipWith addFVars
                  (S.varNamesI n)
                  [1 .. n + 1]
  where
  addFVars v n = ((S.var v, Set.singleton $ S.Var v), n)

mutBin :: S.BinSCon -> Q.QTau -> Q.QTau -> Mut S.Tau
mutBin con q1 q2 = do
                      (s1, vs1) <- mutator q1
                      (s2, vs2) <- mutator q2
                      return (con s1 s2, vs1 `Set.union` vs2)

mutApp :: Q.QTau -> Mut S.Tau
mutApp q = do
  (s, vs) <- mutator q
  let ss = combineTrivAugmenter s vs
  lift $ [ (tc s, vs) | tc <- knownTyCons, (s, vs) <- ss]

-- ******************************************
-- Some concrete type names we use for SModel
-- ******************************************

knownTypes :: [S.Tau]
knownTypes = map S.name ["Int", "Number"]

knownTyCons :: [S.Tau -> S.Tau]
knownTyCons = map (S.A . S.name) ["Ref", "Val"]

-- ************************************************
-- Augment a type ~= bind all of its free variables
-- ************************************************

-- | Try every combination of vars to trivial-augment the type with
combineTrivAugmenter :: S.Tau -> Vars -> [(S.Tau, Vars)]
combineTrivAugmenter t vs = let
  ds = deconcatenations (Set.toList vs)
  bindVars (bvs, fvs) = (trivialAugmenter t bvs, Set.fromList fvs)
  in map bindVars ds

-- | Add where-nodes (L) at the root to bind all given variable names
trivialAugmenter :: S.Tau -> [S.Var] -> S.Tau
trivialAugmenter s vs = foldr f s vs where
                        f v t = S.L v S.B S.T t
