{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, TypeOperators #-}

--  | This compares the speed of 'Data.IntMap' functions with the speed of
--  'Data.EnumMapMap.Strict' functions.

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad.Trans (liftIO)
import           Criterion.Config
import           Criterion.Main
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.EnumMapMap.Strict(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Strict as EMM

newtype ID = ID Int
    deriving (Eq, Enum, Show, Num)

main :: IO()
main = do
    let im :: IntMap Int
        im = IM.fromAscList elemsIm
        emm :: EnumMapMap (K ID) Int
        emm = EMM.fromList elemsEmm
    defaultMainWith
        defaultConfig
        (liftIO . evaluate $ rnf $ im :& emm)
        [ bench "lookup IM" $ whnf (lookupIm keysIm) im
        , bench "lookup EMM" $ whnf (lookupEmm keysEmm) emm
        , bench "insert IM" $ nf (insIm elemsIm) IM.empty
        , bench "insert EMM" $ nf (insEmm elemsEmm) EMM.empty
        , bench "delete IM" $ nf (delIm keysIm) im
        , bench "delete EMM" $ nf (delEmm keysEmm) emm
        ]
    where
      elemsIm = zip keysIm values
      elemsEmm = zip keysEmm values
      keysIm :: [Int]
      keysIm = [1..2^12]
      keysEmm :: [K ID]
      keysEmm = map (K . ID) $ [1..2^12]
      values :: [Int]
      values = [1..2^12]

lookupIm :: [Int] -> IntMap Int -> Int
lookupIm xs im = foldl' (\n k -> fromMaybe n (IM.lookup k im)) 0 xs

lookupEmm :: [K ID] -> EnumMapMap (K ID) Int -> Int
lookupEmm xs emm = foldl' (\n k -> fromMaybe n (EMM.lookup k emm)) 0 xs

insIm :: [(Int, Int)] -> IntMap Int -> IntMap Int
insIm !xs m = foldl' (\m (k, v) -> IM.insert k v m) m xs

insEmm :: [(K ID, Int)] -> EnumMapMap (K ID) Int -> EnumMapMap (K ID) Int
insEmm !xs m = foldl' (\m (k, v) -> EMM.insert k v m) m xs

delIm :: [Int] -> IntMap Int -> IntMap Int
delIm !xs m = foldl' (\m k -> IM.delete k m) m xs

delEmm :: [K ID] -> EnumMapMap (K ID) Int -> EnumMapMap (K ID) Int
delEmm !xs m = foldl' (\m k -> EMM.delete k m) m xs
