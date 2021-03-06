{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This uses QuickCheck to try to check that an 'EnumMapSet'
-- behaves in the same way as an 'IntSet'.

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ((==>))

import qualified Data.IntSet as IS
import qualified Data.List as L

import           Data.EnumMapSet(EnumMapSet, (:&)(..), S(..))
import qualified Data.EnumMapSet as EMS

type TestSet1 = EnumMapSet (S Int)
type TestSet2 = EnumMapSet (Int :& S Int)
type TestSet3 = EnumMapSet (Int :& Int :& S Int)

list2l1 :: [Int] -> [S Int]
list2l1 = map S

list2l2 :: Int -> [Int] -> [Int :& S Int]
list2l2 k1 = map (\k -> k :& S k1)

list2l3 :: Int -> Int -> [Int] -> [Int :& Int :& S Int]
list2l3 k1 k2 = map (\k -> k :& k1 :& S k2)

unKey1 :: S k -> k
unKey1 (S k) = k
unKey2 :: k1 :& S k2 -> k1
unKey2 (k :& S _) = k
unKey3 :: k1 :& k2 :& S k3 -> k1
unKey3 (k :& _ :& S _) = k

runProp :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet1 -> t)
        -> [Int]
        -> Bool
runProp f g list =
    f (IS.fromList list) == g (EMS.fromList $ list2l1 list)

runPropDuo1 :: Eq t =>
              (IS.IntSet -> IS.IntSet -> t)
           -> (TestSet1 -> TestSet1 -> t)
           -> [Int]
           -> [Int]
           -> Bool
runPropDuo1 f g list1 list2
    = f (IS.fromList list1) (IS.fromList list2)
      == g (EMS.fromList $ list2l1 list1) (EMS.fromList $ list2l1 list2)

runProp2 :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet2 -> t)
        -> Int
        -> [Int]
        -> Bool
runProp2 f g k1 list
    = f (IS.fromList list) == g (EMS.fromList $ list2l2 k1 list)

runPropDuo2 :: Eq t =>
              (IS.IntSet -> IS.IntSet -> t)
           -> (TestSet2 -> TestSet2 -> t)
           -> Int
           -> [Int]
           -> [Int]
           -> Bool
runPropDuo2 f g k1 list1 list2
    = f (IS.fromList list1) (IS.fromList list2)
      == g (EMS.fromList $ list2l2 k1 list1)
             (EMS.fromList $ list2l2 k1 list2)

runProp3 :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet3 -> t)
        -> Int -> Int
        -> [Int]
        -> Bool
runProp3 f g k1 k2 list
    = f (IS.fromList list) == g (EMS.fromList $ list2l3 k1 k2 list)

runPropDuo3 :: Eq t =>
              (IS.IntSet -> IS.IntSet -> t)
           -> (TestSet3 -> TestSet3 -> t)
           -> Int -> Int
           -> [Int]
           -> [Int]
           -> Bool
runPropDuo3 f g k1 k2 list1 list2
    = f (IS.fromList list1) (IS.fromList list2)
      == g (EMS.fromList $ list2l3 k1 k2 list1)
             (EMS.fromList $ list2l3 k1 k2 list2)

runPropL :: (IS.IntSet -> IS.IntSet)
         -> (TestSet1 -> TestSet1)
         -> [Int]
         -> Bool
runPropL f g
    = runProp (list2l1 . IS.toList . f) (EMS.toList . g)

runPropDuoL1 :: (IS.IntSet -> IS.IntSet -> IS.IntSet)
             -> (TestSet1 -> TestSet1 -> TestSet1)
             -> [Int]
             -> [Int]
             -> Bool
runPropDuoL1 f g =
    runPropDuo1 (\a b -> list2l1 $ IS.toList $ f a b)
                    (\a b -> EMS.toList $ g a b)

runPropL2 :: (IS.IntSet -> IS.IntSet)
         -> (TestSet2 -> TestSet2)
         -> Int
         -> [Int]
         -> Bool
runPropL2 f g k1
    = runProp2 (list2l2 k1 . IS.toList . f) (EMS.toList . g) k1

runPropDuoL2 :: (IS.IntSet -> IS.IntSet -> IS.IntSet)
             -> (TestSet2 -> TestSet2 -> TestSet2)
             -> Int
             -> [Int]
             -> [Int]
             -> Bool
runPropDuoL2 f g k1 =
    runPropDuo2 (\a b -> list2l2 k1 $ IS.toList $ f a b)
                    (\a b -> EMS.toList $ g a b) k1

runPropL3 :: (IS.IntSet -> IS.IntSet)
         -> (TestSet3 -> TestSet3)
         -> Int -> Int
         -> [Int]
         -> Bool
runPropL3 f g k1 k2
    = runProp3 (list2l3 k1 k2 . IS.toList . f) (EMS.toList . g) k1 k2

runPropDuoL3 :: (IS.IntSet -> IS.IntSet -> IS.IntSet)
             -> (TestSet3 -> TestSet3 -> TestSet3)
             -> Int -> Int
             -> [Int]
             -> [Int]
             -> Bool
runPropDuoL3 f g k1 k2 =
    runPropDuo3 (\a b -> list2l3 k1 k2 $ IS.toList $ f a b)
                    (\a b -> EMS.toList $ g a b) k1 k2

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
          runPropL (IS.insert k) (EMS.insert $ S k)
      prop "Level 2" $ \k k1 ->
          runPropL2 (IS.insert k) (EMS.insert $ k :& S k1) k1
      prop "Level 3" $ \k k1 k2 ->
          runPropL3 (IS.insert k) (EMS.insert $ k :& k1 :& S k2) k1 k2

    describe "delete" $ do
      prop "Level 1" $ \k ->
          runPropL (IS.delete k) (EMS.delete $ S k)
      prop "Level 2" $ \k k1 ->
          runPropL2 (IS.delete k) (EMS.delete $ k :& S k1) k1
      prop "Level 3" $ \k k1 k2 ->
          runPropL3 (IS.delete k) (EMS.delete $ k :& k1 :& S k2) k1 k2

    describe "map" $ do
      let f a = a + 1
      prop "Level 1" $
           runPropL (IS.map f) (EMS.map (\(S k) -> S $ f k))
      prop "Level 2" $
           runPropL2 (IS.map f) (EMS.map (\(k :& S k1) -> f k :& S k1))
      prop "Level 3" $
           runPropL3 (IS.map f)
                         (EMS.map (\(k :& k2 :& S k1) -> f k :& k2 :& S k1))

    describe "findMin" $ do
      prop "Level 1" $ \list ->
          (not $ L.null list) ==>
              runProp (IS.findMin) (unKey1 . EMS.findMin) list
      prop "Level 2" $ \k1 list ->
          (not $ L.null list) ==>
              runProp2 (IS.findMin) (unKey2 . EMS.findMin) k1 list
      prop "Level 3" $ \k1 k2 list ->
          (not $ L.null list) ==>
              runProp3 (IS.findMin) (unKey3 . EMS.findMin) k1 k2 list

    describe "minView" $ do
        let goe _ Nothing = Nothing
            goe f (Just (k, ems)) = Just (f k, EMS.toList ems)
            goi _ Nothing = Nothing
            goi f (Just (k, is)) = Just (k, f $ IS.toList is)
        prop "Level 1" $
             runProp (goi list2l1 . IS.minView) (goe unKey1 . EMS.minView)
        prop "Level 2" $ \k1 ->
             runProp2 (goi (list2l2 k1) . IS.minView)
                          (goe unKey2 . EMS.minView) k1
        prop "Level 3" $ \k1 k2 ->
             runProp3 (goi (list2l3 k1 k2) . IS.minView)
                          (goe unKey3 . EMS.minView) k1 k2

    describe "union" $ do
      prop "Level 1" $
           runPropDuoL1 IS.union EMS.union
      prop "Level 2" $
           runPropDuoL2 IS.union EMS.union
      prop "Level 3" $
           runPropDuoL3 IS.union EMS.union

    describe "difference" $ do
      prop "Level 1" $
           runPropDuoL1 IS.difference EMS.difference
      prop "Level 2" $
           runPropDuoL2 IS.difference EMS.difference
      prop "Level 3" $
           runPropDuoL3 IS.difference EMS.difference

    describe "intersection" $ do
      prop "Level 1" $
           runPropDuoL1 IS.intersection EMS.intersection
      prop "Level 2" $
           runPropDuoL2 IS.intersection EMS.intersection
      prop "Level 3" $
           runPropDuoL3 IS.intersection EMS.intersection

    describe "equals" $ do
      prop "Level 1" $
           runPropDuo1 (==) (==)
      prop "Level 2" $
           runPropDuo2 (==) (==)
      prop "Level 3" $
           runPropDuo3 (==) (==)

    describe "nequals" $ do
      prop "Level 1" $
           runPropDuo1 (/=) (/=)
      prop "Level 2" $
           runPropDuo2 (/=) (/=)
      prop "Level 3" $
           runPropDuo3 (/=) (/=)
