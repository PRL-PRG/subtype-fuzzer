{-|
Module      : SModel
Description : Model for Julia PL's types, somewhat simplified
License     : GPL-3
Maintainer  : a.pelenitsyn@email.com
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE DeriveDataTypeable #-}

module SModel where

import qualified Data.Set as S
import Data.Typeable
import Data.Foldable (find)
import Control.Monad (guard)

-- | "Known" types
known :: [(String, String)]
known = 
    [ ("Int", "Number")
    , ("Number", "Any")
    , ("Vector", "Any")
    , ("Dict", "Any")
    ]

-- | Kinds of known types
k :: [(String, Int)]
k = 
    [ ("Int", 0)
    , ("Number", 0)
    , ("Vector", 1)
    , ("Dict", 2)
    ]

newtype TName = TName String deriving (Eq)
instance Show TName where
    show (TName s) = s

-- **********************
-- Var-related machinery
-- **********************

data Nat = Z | S Nat 
         deriving (Typeable, Eq, Ord)

nat2bin :: Nat -> Int
nat2bin Z = 0
nat2bin (S n) = 1 + nat2bin n

bin2nat :: Int -> Nat
bin2nat i = if i <= 0 then Z else iterate S Z !! i

instance Enum Nat where
    toEnum = bin2nat
    fromEnum = nat2bin

instance Num Nat where
    n + m = case n of { Z -> m; S n' -> n' + m }
    n * m = if n <= 0 || m <= 0 then Z else iterate (+ m) Z !! nat2bin n
    abs = id
    signum n = case n of { Z -> Z; S _ -> S Z}
    fromInteger = bin2nat . fromIntegral
    negate _ = Z

newtype Var = -- VT | VT1 | VS -- 
    Var Nat
            deriving (Eq, Ord)
vNames :: [String]
vNames = ["T", "S"]

instance Show Var where
    show (Var n) = 
        let i = nat2bin n 
        in if i < 2 then vNames !! i else "T" ++ show (i - 1)
{-
    show VT = "T"
    show VT1 = "T1"
    show VS = "S"
-}

varNamesN :: Nat -> [Nat]
varNamesN n = [Z .. n]

varNamesI :: Int -> [Nat]
varNamesI = varNamesN . bin2nat

-- ***********************
-- (Simple) Model of types
-- ***********************
data Tau = T
         | B
         | N TName
         | V Var
         | A Tau Tau
         | L Var Tau Tau Tau
         | P Tau Tau
         | U Tau Tau
         deriving (Typeable, Eq)

-- | Binary constructor from the SModel algebra (A, P, U)
type BinSCon = Tau -> Tau -> Tau

name :: String -> Tau
name = N . TName

var :: Nat -> Tau
var = V . Var

instance Show Tau where
    show T         = "Any"
    show B         = "Union{}"
    show (N tn)    = show tn
    show (V v)     = show v
    show (A t1 t2) = show t1 ++ "{" ++ show t2 ++ "}" 
    show (L v lb ub t)   = "(" ++ show t ++ " where " 
                            ++ (if lb == B then "" else show lb ++ " <: ")
                            ++ show v 
                            ++ (if ub == T then "" else " <: " ++ show ub)
                            ++ ")"
    show (P t1 t2) = "Tuple{" ++ show t1 ++ ", " ++ show t2 ++ "}"
    show (U t1 t2) = "Union{" ++ show t1 ++ ", " ++ show t2 ++ "}"
    
newtype Kind = K { unK :: Int} deriving Eq
mbK0 :: Maybe Kind
mbK0 = Just $ K 0

type G = [Var]
-- | Tau is well-typed in the context Gamma
wt :: G -> Tau -> Maybe Kind
wt _ T         = mbK0
wt _ B         = mbK0
wt _ (N tn)    = K <$> show tn `lookup` k
wt g (V v)     = find (== v) g >> mbK0
wt g (A t1 t2)   = do 
    k1 <- wt g t1
    k2 <- wt g t2
    guard $ unK k1 > 0
    guard $ unK k2 == 0
    return . K $ unK k1 - 1
wt g (L v lb ub t)   = wt g lb >> wt g ub >> wt (v : g) t
wt g (P t1 t2) = wt g t1 >> wt g t2 >> mbK0
wt g (U t1 t2) = wt g t1 >> wt g t2 >> mbK0
