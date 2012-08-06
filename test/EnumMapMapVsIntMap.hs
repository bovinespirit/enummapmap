{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Test.Hspec.QuickCheck (prop)
import           Test.Hspec.Monadic
import           Test.QuickCheck ()

import qualified Data.IntMap as IM

import qualified Data.EnumMapMap.Base as EMM

type TestMap  = EMM.EnumMapMap (EMM.Key1 Int)             Int
type TestMap2 = EMM.EnumMapMap (EMM.Key2 Int Int)         Int
type TestMap3 = EMM.EnumMapMap (EMM.Key3 Int Int Int)     Int
type TestMap4 = EMM.EnumMapMap (EMM.Key4 Int Int Int Int) Int

list2l2 :: Int -> [(Int, Int)] -> [(Int, [(Int, Int)])]
list2l2 k1 = map f
    where
      f (a, b) = (a, [(k1, b)])

list2l3 :: Int -> Int -> [(Int, Int)] -> [(Int, [(Int, [(Int, Int)])])]
list2l3 k1 k2 = map f
    where
      f (a, b) = (a, [(k1, [(k2, b)])])

list2l4 :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, [(Int, [(Int, [(Int, Int)])])])]
list2l4 k1 k2 k3 = map f
    where
      f (a, b) = (a, [(k1, [(k2, [(k3, b)])])])

-- | Run functions on an 'IntMap' and an 'EnumMapMap' created from list and check
-- that the results are equal
runProp :: Eq t =>
           (IM.IntMap Int -> t)
        -> (TestMap -> t)
        -> [(Int, Int)]
        -> Bool
runProp f g list =
    (f $ IM.fromList list) == (g $ EMM.fromList1 list)

runProp2 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap2 -> t)
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp2 f g k1 list =
    (f $ IM.fromList list) == (g $ EMM.fromList2 $ list2l2 k1 list)

runProp3 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap3 -> t)
         -> Int
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp3 f g k1 k2 list =
    (f $ IM.fromList list) == (g $ EMM.fromList3 $ list2l3 k1 k2 list)

runProp4 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap4 -> t)
         -> Int
         -> Int
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp4 f g k1 k2 k3 list =
    (f $ IM.fromList list) == (g $ EMM.fromList4 $ list2l4 k1 k2 k3 list)

-- | Run functions on an 'IntMap' and an 'EnumMapMap' created from 'list' and check
-- that the resulting 'IntMap' and 'EnumMapMap' are equal
runPropL :: (IM.IntMap Int -> IM.IntMap Int)
         -> (TestMap -> TestMap)
         -> [(Int, Int)]
         -> Bool
runPropL f g =
    runProp (IM.toList . f) (EMM.toList1 . g)

runPropL2 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap2 -> TestMap2)
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL2 f g k1 =
    runProp2 (list2l2 k1 . IM.toList . f) (EMM.toList2 . g) k1

runPropL3 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap3 -> TestMap3)
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL3 f g k1 k2 =
    runProp3 (list2l3 k1 k2 . IM.toList . f) (EMM.toList3 . g) k1 k2

runPropL4 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap4 -> TestMap4)
          -> Int
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL4 f g k1 k2 k3 =
    runProp4 (list2l4 k1 k2 k3 . IM.toList . f) (EMM.toList4 . g) k1 k2 k3

main :: IO ()
main = hspecX $ do
    describe "toList fromList" $ do
        prop "Level 1" $
             runPropL id id
        prop "Level 2" $
             runPropL2 id id
        prop "Level 3" $
             runPropL3 id id
        prop "Level 4" $
             runPropL4 id id

    describe "lookup" $ do
        prop "Level 1" $ \i ->
            runProp (IM.lookup i) (EMM.lookup $ EMM.Key1 i)
        prop "Level 2" $ \i k1 ->
            runProp2 (IM.lookup i) (EMM.lookup $ EMM.Key2 i k1) k1
        prop "Level 3" $ \i k1 k2 ->
            runProp3 (IM.lookup i) (EMM.lookup $ EMM.Key3 i k1 k2) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
            runProp4 (IM.lookup i) (EMM.lookup $ EMM.Key4 i k1 k2 k3) k1 k2 k3

    describe "member" $ do
        prop "Level 1" $ \i ->
            runProp (IM.member i) (EMM.member $ EMM.Key1 i)
        prop "Level 2" $ \i k1 ->
            runProp2 (IM.member i) (EMM.member $ EMM.Key2 i k1) k1
        prop "Level 3" $ \i k1 k2 ->
            runProp3 (IM.member i) (EMM.member $ EMM.Key3 i k1 k2) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
            runProp4 (IM.member i) (EMM.member $ EMM.Key4 i k1 k2 k3) k1 k2 k3

    describe "insert" $ do
        prop "Level 1" $ \ i j ->
             runPropL (IM.insert i j) (EMM.insert (EMM.Key1 i) j)
        prop "Level 2" $ \ i j k1 ->
             runPropL2 (IM.insert i j) (EMM.insert (EMM.Key2 i k1) j) k1
        prop "Level 3" $ \ i j k1 k2 ->
             runPropL3 (IM.insert i j) (EMM.insert (EMM.Key3 i k1 k2) j) k1 k2
        prop "Level 4" $ \ i j k1 k2 k3 ->
             runPropL4 (IM.insert i j) (EMM.insert (EMM.Key4 i k1 k2 k3) j) k1 k2 k3
