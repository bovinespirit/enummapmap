{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This uses QuickCheck to try to check that an 'EnumMapMap'
-- behaves in the same way as an 'IntMap'.  It checks each level of the
-- 'EnumMapMap' one by one for each function.  The tests are actually fairly
-- straightforward, as it only checks one level at a time.

import           Test.Hspec.Monadic
import           Test.Hspec.QuickCheck (prop)
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

-- | Run functions on 2 'IntMap's and 2 'EnumMapMap's created from lists and check
-- that the results are equal
runPropDuo :: Eq t =>
           (IM.IntMap Int -> IM.IntMap Int -> t)
        -> (TestMap -> TestMap -> t)
        -> [(Int, Int)]
        -> [(Int, Int)]
        -> Bool
runPropDuo f g list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList1 list1) $ EMM.fromList1 list2)

runProp2 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap2 -> t)
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp2 f g k1 list =
    (f $ IM.fromList list) == (g $ EMM.fromList2 $ list2l2 k1 list)

runPropDuo2 :: Eq t =>
               (IM.IntMap Int -> IM.IntMap Int -> t)
            -> (TestMap2 -> TestMap2 -> t)
            -> Int
            -> [(Int, Int)]
            -> [(Int, Int)]
            -> Bool
runPropDuo2 f g k1 list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList2 $ list2l2 k1 list1) $
          EMM.fromList2 $ list2l2 k1 list2)

runProp3 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap3 -> t)
         -> Int
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp3 f g k1 k2 list =
    (f $ IM.fromList list) == (g $ EMM.fromList3 $ list2l3 k1 k2 list)

runPropDuo3 :: Eq t =>
               (IM.IntMap Int -> IM.IntMap Int -> t)
            -> (TestMap3 -> TestMap3 -> t)
            -> Int
            -> Int
            -> [(Int, Int)]
            -> [(Int, Int)]
            -> Bool
runPropDuo3 f g k1 k2 list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList3 $ list2l3 k1 k2 list1) $
          EMM.fromList3 $ list2l3 k1 k2 list2)

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

runPropDuo4 :: Eq t =>
               (IM.IntMap Int -> IM.IntMap Int -> t)
            -> (TestMap4 -> TestMap4 -> t)
            -> Int
            -> Int
            -> Int
            -> [(Int, Int)]
            -> [(Int, Int)]
            -> Bool
runPropDuo4 f g k1 k2 k3 list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList4 $ list2l4 k1 k2 k3 list1) $
          EMM.fromList4 $ list2l4 k1 k2 k3 list2)

-- | Run functions on an 'IntMap' and an 'EnumMapMap' created from 'list' and check
-- that the resulting 'IntMap' and 'EnumMapMap' are equal
runPropL :: (IM.IntMap Int -> IM.IntMap Int)
         -> (TestMap -> TestMap)
         -> [(Int, Int)]
         -> Bool
runPropL f g =
    runProp (IM.toList . f) (EMM.toList1 . g)

runPropDuoL :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
            -> (TestMap -> TestMap -> TestMap)
         -> [(Int, Int)]
         -> [(Int, Int)]
         -> Bool
runPropDuoL f g =
    runPropDuo (\a b -> IM.toList $ f a b)
                   (\a b -> EMM.toList1 $ g a b)

runPropL2 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap2 -> TestMap2)
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL2 f g k1 =
    runProp2 (list2l2 k1 . IM.toList . f) (EMM.toList2 . g) k1

runPropDuoL2 :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
             -> (TestMap2 -> TestMap2 -> TestMap2)
             -> Int
             -> [(Int, Int)]
             -> [(Int, Int)]
             -> Bool
runPropDuoL2 f g k1 =
    runPropDuo2 (\a b -> list2l2 k1 $ IM.toList $ f a b)
                    (\a b -> EMM.toList2 $ g a b) k1

runPropL3 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap3 -> TestMap3)
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL3 f g k1 k2 =
    runProp3 (list2l3 k1 k2 . IM.toList . f) (EMM.toList3 . g) k1 k2

runPropDuoL3 :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
             -> (TestMap3 -> TestMap3 -> TestMap3)
             -> Int
             -> Int
             -> [(Int, Int)]
             -> [(Int, Int)]
             -> Bool
runPropDuoL3 f g k1 k2 =
    runPropDuo3 (\a b -> list2l3 k1 k2 $ IM.toList $ f a b)
                    (\a b -> EMM.toList3 $ g a b) k1 k2

runPropL4 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap4 -> TestMap4)
          -> Int
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL4 f g k1 k2 k3 =
    runProp4 (list2l4 k1 k2 k3 . IM.toList . f) (EMM.toList4 . g) k1 k2 k3

runPropDuoL4 :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
             -> (TestMap4 -> TestMap4 -> TestMap4)
             -> Int
             -> Int
             -> Int
             -> [(Int, Int)]
             -> [(Int, Int)]
             -> Bool
runPropDuoL4 f g k1 k2 k3 =
    runPropDuo4 (\a b -> list2l4 k1 k2 k3 $ IM.toList $ f a b)
                    (\a b -> EMM.toList4 $ g a b) k1 k2 k3

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
        prop "Level 1" $ \i j ->
             runPropL (IM.insert i j) (EMM.insert (EMM.Key1 i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insert i j) (EMM.insert (EMM.Key2 i k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insert i j) (EMM.insert (EMM.Key3 i k1 k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insert i j)
                           (EMM.insert (EMM.Key4 i k1 k2 k3) j) k1 k2 k3

    describe "insertWith" $ do
        prop "Level 1" $ \i j ->
             runPropL (IM.insertWith (+) i j) $
                          (EMM.insertWith (+) (EMM.Key1 i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (EMM.Key2 i k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (EMM.Key3 i k1 k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (EMM.Key4 i k1 k2 k3) j) k1 k2 k3

    describe "insertWithKey" $ do
        let f a b c = a + b + c
        prop "Level 1" $ \i j ->
             runPropL (IM.insertWithKey f i j) $
                          (EMM.insertWithKey
                           (\(EMM.Key1 k) -> f k)
                           (EMM.Key1 i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(EMM.Key2 k _) -> f k)
                            (EMM.Key2 i k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(EMM.Key3 k _ _) -> f k)
                            (EMM.Key3 i k1 k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(EMM.Key4 k _ _ _) -> f k)
                            (EMM.Key4 i k1 k2 k3) j) k1 k2 k3

    describe "delete" $ do
        prop "Level 1" $ \i ->
             runPropL (IM.delete i) (EMM.delete (EMM.Key1 i))
        prop "Level 2" $ \i k1 ->
             runPropL2 (IM.delete i) (EMM.delete (EMM.Key2 i k1)) k1
        prop "Level 3" $ \i k1 k2 ->
             runPropL3 (IM.delete i) (EMM.delete (EMM.Key3 i k1 k2)) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
             runPropL4 (IM.delete i)
                           (EMM.delete (EMM.Key4 i k1 k2 k3)) k1 k2 k3

    describe "foldrWithKey" $ do
        let f :: Int -> Int -> [Int] -> [Int]
            f a b c = [a + b] ++ c
        prop "Level 1" $
             runProp (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(EMM.Key1 k) -> f $ toEnum k) [])
        prop "Level 2" $
             runProp2 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(EMM.Key2 k _) -> f $ toEnum k) [])
        prop "Level 3" $
             runProp3 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(EMM.Key3 k _ _) -> f $ toEnum k) [])
        prop "Level 3" $
             runProp4 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(EMM.Key4 k _ _ _) -> f $ toEnum k) [])

    describe "map" $ do
        let f a = a + 1
        prop "Level 1" $
             runPropL (IM.map f) (EMM.map1 f)
        prop "Level 2" $
             runPropL2 (IM.map f) (EMM.map2 f)
        prop "Level 3" $
             runPropL3 (IM.map f) (EMM.map3 f)
        prop "Level 4" $
             runPropL4 (IM.map f) (EMM.map4 f)

    describe "mapWithKey" $ do
        let f :: Int -> Int -> Int
            f k a = k + a
        prop "Level 1" $
             runPropL  (IM.mapWithKey f) (EMM.mapWithKey
                                          (\(EMM.Key1 k) -> f $ toEnum k))
        prop "Level 2" $
             runPropL2 (IM.mapWithKey f) (EMM.mapWithKey
                                          (\(EMM.Key2 k _) -> f $ toEnum k))
        prop "Level 3" $
             runPropL3 (IM.mapWithKey f) (EMM.mapWithKey
                                          (\(EMM.Key3 k _ _) -> f $ toEnum k))
        prop "Level 4" $
             runPropL4 (IM.mapWithKey f) (EMM.mapWithKey
                                          (\(EMM.Key4 k _ _ _) -> f $ toEnum k))

    describe "union" $ do
        prop "Level 1" $
             runPropDuoL  IM.union EMM.union1
        prop "Level 2" $
             runPropDuoL2 IM.union EMM.union2
        prop "Level 3" $
             runPropDuoL3 IM.union EMM.union3
        prop "Level 4" $
             runPropDuoL4 IM.union EMM.union4

    describe "unionWith" $ do
        prop "Level 1" $
             runPropDuoL  (IM.unionWith (+)) (EMM.unionWith1 (+))
        prop "Level 2" $
             runPropDuoL2 (IM.unionWith (+)) (EMM.unionWith2 (+))
        prop "Level 3" $
             runPropDuoL3 (IM.unionWith (+)) (EMM.unionWith3 (+))
        prop "Level 4" $
             runPropDuoL4 (IM.unionWith (+)) (EMM.unionWith4 (+))

    describe "unionWithKey" $ do
        let f :: Int -> Int -> Int -> Int
            f a b c = (a + b) * c
        prop "Level 1" $
             runPropDuoL (IM.unionWithKey f)
                              (EMM.unionWithKey
                                      (\(EMM.Key1 k) -> f $ toEnum k))
        prop "Level 2" $
             runPropDuoL2 (IM.unionWithKey f)
                              (EMM.unionWithKey
                                      (\(EMM.Key2 k _) -> f $ toEnum k))
        prop "Level 3" $
             runPropDuoL3 (IM.unionWithKey f)
                              (EMM.unionWithKey
                                      (\(EMM.Key3 k _ _) -> f $ toEnum k))
        prop "Level 4" $
             runPropDuoL4 (IM.unionWithKey f)
                              (EMM.unionWithKey
                                      (\(EMM.Key4 k _ _ _) -> f $ toEnum k))

    describe "intersectionWithKey" $ do
        let f :: Int -> Int -> Int -> Int
            f a b c = (a + b) * c
        prop "Level 1" $
             runPropDuoL (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey1
                                     (\(EMM.Key1 k) -> f $ toEnum k))
        prop "Level 2" $
             runPropDuoL2 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey2
                                     (\(EMM.Key2 k _) -> f $ toEnum k))
        prop "Level 3" $
             runPropDuoL3 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey3
                                     (\(EMM.Key3 k _ _) -> f $ toEnum k))
        prop "Level 4" $
             runPropDuoL4 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey4
                                     (\(EMM.Key4 k _ _ _) -> f $ toEnum k))

    describe "intersectionWith" $ do
        prop "Level 1" $
             runPropDuoL (IM.intersectionWith (*))
                             (EMM.intersectionWith1 (*))
        prop "Level 2" $
             runPropDuoL2 (IM.intersectionWith (*))
                             (EMM.intersectionWith2 (*))
        prop "Level 3" $
             runPropDuoL3 (IM.intersectionWith (*))
                             (EMM.intersectionWith3 (*))
        prop "Level 4" $
             runPropDuoL4 (IM.intersectionWith (*))
                             (EMM.intersectionWith4 (*))

    describe "intersection" $ do
        prop "Level 1" $
             runPropDuoL (IM.intersection)
                             (EMM.intersection1)
        prop "Level 2" $
             runPropDuoL2 (IM.intersection)
                             (EMM.intersection2)
        prop "Level 3" $
             runPropDuoL3 (IM.intersection)
                             (EMM.intersection3)
        prop "Level 4" $
             runPropDuoL4 (IM.intersection)
                             (EMM.intersection4)

    describe "differenceWithKey" $ do
        let f :: Int -> Int -> Int -> Maybe Int
            f a b c = Just $ (a + b) * c
        prop "Level 1" $
             runPropDuoL (IM.differenceWithKey f)
                             (EMM.differenceWithKey1
                                     (\(EMM.Key1 k) -> f $ toEnum k))
        prop "Level 2" $
             runPropDuoL2 (IM.differenceWithKey f)
                             (EMM.differenceWithKey2
                                     (\(EMM.Key2 k _) -> f $ toEnum k))
        prop "Level 3" $
             runPropDuoL3 (IM.differenceWithKey f)
                             (EMM.differenceWithKey3
                                     (\(EMM.Key3 k _ _) -> f $ toEnum k))
        prop "Level 4" $
             runPropDuoL4 (IM.differenceWithKey f)
                             (EMM.differenceWithKey4
                                     (\(EMM.Key4 k _ _ _) -> f $ toEnum k))

    describe "differenceWith" $ do
        let f a b = Just $ a * b
        prop "Level 1" $
             runPropDuoL (IM.differenceWith f)
                             (EMM.differenceWith1 f)
        prop "Level 2" $
             runPropDuoL2 (IM.differenceWith f)
                             (EMM.differenceWith2 f)
        prop "Level 3" $
             runPropDuoL3 (IM.differenceWith f)
                             (EMM.differenceWith3 f)
        prop "Level 4" $
             runPropDuoL4 (IM.differenceWith f)
                             (EMM.differenceWith4 f)

    describe "difference" $ do
        prop "Level 1" $
             runPropDuoL IM.difference EMM.difference1
        prop "Level 2" $
             runPropDuoL2 IM.difference EMM.difference2
        prop "Level 3" $
             runPropDuoL3 IM.difference EMM.difference3
        prop "Level 4" $
             runPropDuoL4 IM.difference EMM.difference4

