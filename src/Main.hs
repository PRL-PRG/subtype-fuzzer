{-# LANGUAGE TemplateHaskell #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
import           QModel           (QTau)
import           Mutation

--import Text.PrettyPrint.Annotated.WL

--import Test.QuickCheck (sample')
import Control.Monad (join)

--import Control.Search
-- import Control.Enumerable.Values (values)
import Test.Feat
--------------------------------------------------------------------------------

deriveEnumerable ''QTau

-- | A number of "experiments" (the same as the number of quasitypes)
exp_num :: Int
exp_num = 99

qts :: [QTau]
qts = take exp_num . concat . map snd $ vs where
    vs = values

-- | Entry point: print ya some random types
main :: IO ()
main = do
    let ts = map mutate qts
    mapM_ print ts
    putStrLn "bye"
