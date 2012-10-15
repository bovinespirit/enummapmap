{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This uses QuickCheck to try to check that an 'EnumMapSet'
-- behaves in the same way as an 'IntSet'.

import           Test.Hspec.Monadic
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ()

import qualified Data.IntSet as IS

import           Data.EnumMapSet(EnumMapSet, (:&)(..), K(..))
import qualified Data.EnumMapSet as EMS

type TestSet1 = EnumMapSet (K Int)
type TestSet2 = EnumMapSet (Int :& K Int)
type TestSet3 = EnumMapSet (Int :& Int :& K Int)

list2l1 :: [Int] -> [K Int]
list2l1 = map (\k -> K k)

list2l2 :: Int -> [Int] -> [Int :& K Int]
list2l2 k1 = map (\k -> k :& K k1)

list2l3 :: Int -> Int -> [Int] -> [Int :& Int :& K Int]
list2l3 k1 k2 = map (\k -> k :& k1 :& K k2)

runProp :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet1 -> t)
        -> [Int]
        -> Bool
runProp f g list =
    (f $ IS.fromList list) == (g $ EMS.fromList $ list2l1 list)

runProp2 :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet2 -> t)
        -> Int
        -> [Int]
        -> Bool
runProp2 f g k1 list
    = (f $ IS.fromList list) == (g $ EMS.fromList $ list2l2 k1 list)

runProp3 :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet3 -> t)
        -> Int -> Int
        -> [Int]
        -> Bool
runProp3 f g k1 k2 list
    = (f $ IS.fromList list) == (g $ EMS.fromList $ list2l3 k1 k2 list)

runPropL :: (IS.IntSet -> IS.IntSet)
         -> (TestSet1 -> TestSet1)
         -> [Int]
         -> Bool
runPropL f g
    = runProp (list2l1 . IS.toList . f) (EMS.toList . g)

runPropL2 :: (IS.IntSet -> IS.IntSet)
         -> (TestSet2 -> TestSet2)
         -> Int
         -> [Int]
         -> Bool
runPropL2 f g k1
    = runProp2 (list2l2 k1 . IS.toList . f) (EMS.toList . g) k1

runPropL3 :: (IS.IntSet -> IS.IntSet)
         -> (TestSet3 -> TestSet3)
         -> Int -> Int
         -> [Int]
         -> Bool
runPropL3 f g k1 k2
    = runProp3 (list2l3 k1 k2 . IS.toList . f) (EMS.toList . g) k1 k2

main :: IO ()
main = hspec $ do
    describe "toList fromList" $ do
      prop "Level 1" $ runPropL id id
      prop "Level 2" $ runPropL2 id id
      prop "Level 3" $ runPropL3 id id

    describe "null" $ do
      prop "Level 1" $ runProp IS.null EMS.null
      prop "Level 2" $ runProp2 IS.null EMS.null
      prop "Level 3" $ runProp3 IS.null EMS.null

    describe "insert" $ do
      prop "Level 1" $ \k ->
          runPropL (IS.insert k) (EMS.insert $ K k)
      prop "Level 2" $ \k k1 ->
          runPropL2 (IS.insert k) (EMS.insert $ k :& K k1) k1
      prop "Level 3" $ \k k1 k2 ->
          runPropL3 (IS.insert k) (EMS.insert $ k :& k1 :& K k2) k1 k2

    describe "delete" $ do
      prop "Level 1" $ \k ->
          runPropL (IS.delete k) (EMS.delete $ K k)
      prop "Level 2" $ \k k1 ->
          runPropL2 (IS.delete k) (EMS.delete $ k :& K k1) k1
      prop "Level 3" $ \k k1 k2 ->
          runPropL3 (IS.delete k) (EMS.delete $ k :& k1 :& K k2) k1 k2
