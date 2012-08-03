{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Test.Hspec.QuickCheck (prop)
import           Test.Hspec.Monadic
import           Test.QuickCheck ()

import           Data.IntMap as IM

import qualified Data.EnumMapMap.Base as EMM

type TestMap = EMM.EnumMapMap (EMM.Key1 Int) Int

runProp :: Eq t =>
           (IM.IntMap Int -> t)
        -> (TestMap -> t)
        -> [(Int, Int)]
        -> Bool
runProp f g list =
    (f $ IM.fromList list) == (g $ EMM.fromList1 list)

runPropL :: (IM.IntMap Int -> IM.IntMap Int)
         -> (TestMap -> TestMap)
         -> [(Int, Int)]
         -> Bool
runPropL f g =
    runProp (IM.toList . f) (EMM.toList1 . g)

main :: IO ()
main = hspecX $ do
         prop "toList fromList" $
             runPropL id id

         prop "lookup" $ \ i ->
             runProp (IM.lookup i) (EMM.lookup (EMM.Key1 i))

         prop "member" $ \ i ->
             runProp (IM.member i) (EMM.member $ EMM.Key1 i)

         prop "insert" $ \ i j ->
             runPropL (IM.insert i j) (EMM.insert (EMM.Key1 i) j)
