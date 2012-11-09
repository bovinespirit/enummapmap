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
        imAr :: [IntMap Int]
        imAr = map IM.fromAscList elemsArIm
        emm :: EnumMapMap (K ID) Int
        emm = EMM.fromList elemsEmm
        emmAr :: [EnumMapMap (K ID) Int]
        emmAr = map EMM.fromList elemsArEmm
    defaultMainWith
        defaultConfig
        (liftIO . evaluate $ rnf $ im :& emm :& imAr :& emmAr)
        [ bench "lookup IM" $ whnf (lookupIm keysIm) im
        , bench "lookup EMM" $ whnf (lookupEmm keysEmm) emm
        , bench "insert IM" $ whnf (insIm elemsIm) IM.empty
        , bench "insert EMM" $ whnf (insEmm elemsEmm) EMM.empty
        , bench "delete IM" $ whnf (delIm keysIm) im
        , bench "delete EMM" $ whnf (delEmm keysEmm) emm
        , bench "union IM" $ nf (dual IM.union im) imAr
        , bench "union EMM" $ nf (dual EMM.union emm) emmAr
        , bench "difference IM" $ nf (dual IM.difference im) imAr
        , bench "difference EMM" $ nf (dual EMM.difference emm) emmAr
        , bench "intersection IM" $ nf (dual IM.intersection im) imAr
        , bench "intersection EMM" $ nf (dual EMM.intersection emm) emmAr
        ]
    where
      elemsIm = zip keysIm values
      elemsEmm = zip keysEmm values
      elemsArIm = map (\ks -> zip ks values) keysArIm
      elemsArEmm = map (\ks -> zip ks values) keysArEmm
      keysIm :: [Int]
      keysIm = [1..2^12]
      keysArIm :: [[Int]]
      keysArIm = do
         i <- [1..2^5]
         return $ do
                   j <- [1..(2^8)]
                   return $ j * i
      keysEmm :: [K ID]
      keysEmm = map (K . ID) keysIm
      keysArEmm :: [[K ID]]
      keysArEmm = map (map (K . ID)) keysArIm
      values :: [Int]
      values = [1..2^12]

lookupIm :: [Int] -> IntMap Int -> Int
lookupIm xs im = foldl' (\n k -> fromMaybe n (IM.lookup k im)) 0 xs

lookupEmm :: [K ID] -> EnumMapMap (K ID) Int -> Int
lookupEmm xs emm = foldl' (\n k -> fromMaybe n (EMM.lookup k emm)) 0 xs

insIm :: [(Int, Int)] -> IntMap Int -> IntMap Int
insIm xs m = foldl' (\m (k, v) -> v `seq` IM.insert k v m) m xs

insEmm :: [(K ID, Int)] -> EnumMapMap (K ID) Int -> EnumMapMap (K ID) Int
insEmm xs m = foldl' (\m (k, v) -> EMM.insert k v m) m xs

delIm :: [Int] -> IntMap Int -> IntMap Int
delIm xs m = foldl' (\m k -> IM.delete k m) m xs

delEmm :: [K ID] -> EnumMapMap (K ID) Int -> EnumMapMap (K ID) Int
delEmm xs m = foldl' (\m k -> EMM.delete k m) m xs

dual :: (a -> a -> a) -> a -> [a] -> [a]
dual f i = map $! f i
