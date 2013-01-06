{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This uses QuickCheck to try to check that an 'EnumMapMap'
-- behaves in the same way as an 'IntMap'.  It checks up to 4 levels of
-- 'EnumMapMap' one by one for each function.  It does not check that empty
-- EnumMapMaps are removed.

import           Test.Hspec

import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck ((==>))

import qualified Data.IntSet as IS
import           Data.EnumMapSet (S(..))
import qualified Data.EnumMapSet as EMS
import qualified Data.List as L

#ifdef LAZY
import qualified Data.IntMap as IM

import           Data.EnumMapMap.Lazy(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Lazy as EMM
#else
import qualified Data.IntMap as IM

import           Data.EnumMapMap.Strict(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Strict as EMM
#endif

type TestMap  = EnumMapMap (K Int)                      Int
type TestMap2 = EnumMapMap (Int :& K Int)               Int
type TestMap3 = EnumMapMap (Int :& Int :& K Int)        Int
type TestMap4 = EnumMapMap (Int :& Int :& Int :& K Int) Int
-- type TestSet  = EnumMapSet (S Int)
-- type TestSet2 = EnumMapSet (Int :& S Int)
-- type TestSet3 = EnumMapSet (Int :& Int :& S Int)
-- type TestSet4 = EnumMapSet (Int :& Int :& Int :& S Int)

list2l1 :: [(Int, Int)] -> [(K Int, Int)]
list2l1 = map (\(a, b) -> (K a, b))

list2l2 :: Int -> [(Int, Int)] -> [(Int :& K Int, Int)]
list2l2 k1 = map (\(a, b) -> (a :& K k1, b))

list2l3 :: Int -> Int -> [(Int, Int)] -> [(Int :& Int :& K Int, Int)]
list2l3 k1 k2 = map (\(a, b) -> (a :& k1 :& K k2, b))

list2l4 :: Int -> Int -> Int -> [(Int, Int)] -> [(Int :& Int :& Int :& K Int, Int)]
list2l4 k1 k2 k3 = map (\(a, b) -> (a :& k1 :& k2 :& K k3, b))

set2l1 :: [Int] -> [S Int]
set2l1 = map S

set2l2 :: Int -> [Int] -> [Int :& S Int]
set2l2 s1 = map (\s -> s :& S s1)

set2l3 :: Int -> Int -> [Int] -> [Int :& Int :& S Int]
set2l3 s1 s2 = map (\s -> s :& s1 :& S s2)

set2l4 :: Int -> Int -> Int -> [Int] -> [Int :& Int :& Int :& S Int]
set2l4 s1 s2 s3 = map (\s -> s :& s1 :& s2 :& S s3)

unKey1 :: K k -> k
unKey1 (K k) = k
unKey2 :: k1 :& K k2 -> k1
unKey2 (k :& K _) = k
unKey3 :: k1 :& k2 :& K k3 -> k1
unKey3 (k :& _ :& K _) = k
unKey4 :: k1 :& k2 :& k3 :& K k4 -> k1
unKey4 (k :& _ :& _ :& K _) = k

-- | Run functions on an 'IntMap' and an 'EnumMapMap' created from list and check
-- that the results are equal
runProp :: Eq t =>
           (IM.IntMap Int -> t)
        -> (TestMap -> t)
        -> [(Int, Int)]
        -> Bool
runProp f g list =
    (f $ IM.fromList list) == (g $ EMM.fromList $ list2l1 list)

runPropDuo :: Eq t =>
           (IM.IntMap Int -> IM.IntMap Int -> t)
        -> (TestMap -> TestMap -> t)
        -> [(Int, Int)]
        -> [(Int, Int)]
        -> Bool
runPropDuo f g list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList $ list2l1 list1) $ EMM.fromList $ list2l1 list2)

runProp2 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap2 -> t)
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp2 f g k1 list =
    (f $ IM.fromList list) == (g $ EMM.fromList $ list2l2 k1 list)

runPropDuo2 :: Eq t =>
               (IM.IntMap Int -> IM.IntMap Int -> t)
            -> (TestMap2 -> TestMap2 -> t)
            -> Int
            -> [(Int, Int)]
            -> [(Int, Int)]
            -> Bool
runPropDuo2 f g k1 list1 list2 =
    (f (IM.fromList list1) $ IM.fromList list2)
    == (g (EMM.fromList $ list2l2 k1 list1) $
          EMM.fromList $ list2l2 k1 list2)

runProp3 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap3 -> t)
         -> Int
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp3 f g k1 k2 list =
    (f $ IM.fromList list) == (g $ EMM.fromList $ list2l3 k1 k2 list)

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
    == (g (EMM.fromList $ list2l3 k1 k2 list1) $
          EMM.fromList $ list2l3 k1 k2 list2)

runProp4 :: Eq t =>
            (IM.IntMap Int -> t)
         -> (TestMap4 -> t)
         -> Int
         -> Int
         -> Int
         -> [(Int, Int)]
         -> Bool
runProp4 f g k1 k2 k3 list =
    (f $ IM.fromList list) == (g $ EMM.fromList $ list2l4 k1 k2 k3 list)

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
    == (g (EMM.fromList $ list2l4 k1 k2 k3 list1) $
          EMM.fromList $ list2l4 k1 k2 k3 list2)

-- | Run functions on an 'IntMap' and an 'EnumMapMap' created from 'list' and check
-- that the resulting 'IntMap' and 'EnumMapMap' are equal
runPropL :: (IM.IntMap Int -> IM.IntMap Int)
         -> (TestMap -> TestMap)
         -> [(Int, Int)]
         -> Bool
runPropL f g =
    runProp (list2l1 . IM.toList . f) (EMM.toList  . g)

runPropDuoL :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
            -> (TestMap -> TestMap -> TestMap)
         -> [(Int, Int)]
         -> [(Int, Int)]
         -> Bool
runPropDuoL f g =
    runPropDuo (\a b -> list2l1 $ IM.toList $ f a b)
                   (\a b -> EMM.toList $ g a b)

runPropL2 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap2 -> TestMap2)
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL2 f g k1 =
    runProp2 (list2l2 k1 . IM.toList . f) (EMM.toList . g) k1

runPropDuoL2 :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
             -> (TestMap2 -> TestMap2 -> TestMap2)
             -> Int
             -> [(Int, Int)]
             -> [(Int, Int)]
             -> Bool
runPropDuoL2 f g k1 =
    runPropDuo2 (\a b -> list2l2 k1 $ IM.toList $ f a b)
                    (\a b -> EMM.toList $ g a b) k1

runPropL3 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap3 -> TestMap3)
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL3 f g k1 k2 =
    runProp3 (list2l3 k1 k2 . IM.toList . f) (EMM.toList . g) k1 k2

runPropDuoL3 :: (IM.IntMap Int -> IM.IntMap Int -> IM.IntMap Int)
             -> (TestMap3 -> TestMap3 -> TestMap3)
             -> Int
             -> Int
             -> [(Int, Int)]
             -> [(Int, Int)]
             -> Bool
runPropDuoL3 f g k1 k2 =
    runPropDuo3 (\a b -> list2l3 k1 k2 $ IM.toList $ f a b)
                    (\a b -> EMM.toList $ g a b) k1 k2

runPropL4 :: (IM.IntMap Int -> IM.IntMap Int)
          -> (TestMap4 -> TestMap4)
          -> Int
          -> Int
          -> Int
          -> [(Int, Int)]
          -> Bool
runPropL4 f g k1 k2 k3 =
    runProp4 (list2l4 k1 k2 k3 . IM.toList . f) (EMM.toList . g) k1 k2 k3

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
                    (\a b -> EMM.toList $ g a b) k1 k2 k3

-- 'fromSet' is not in containers 4.*
-- runSetProp :: Eq t =>
--               (IS.IntSet -> IM.IntMap Int)
--            -> (TestSet -> TestMap)
--            -> [Int]
--            -> Bool
-- runSetProp f g list =
--     (set2l1 $ IM.toList $ f $ IS.fromList list)
--     == (EMM.toList $ g $ EMS.fromList $ set2l1 list)

main :: IO ()
main = hspec $ do
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
            runProp (IM.lookup i) (EMM.lookup $ K i)
        prop "Level 2" $ \i k1 ->
            runProp2 (IM.lookup i) (EMM.lookup $ i :& K k1) k1
        prop "Level 3" $ \i k1 k2 ->
            runProp3 (IM.lookup i) (EMM.lookup $ i :& k1 :& K k2) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
            runProp4 (IM.lookup i) (EMM.lookup $ i :& k1 :& k2 :& K k3) k1 k2 k3

    describe "member" $ do
        prop "Level 1" $ \i ->
            runProp (IM.member i) (EMM.member $ K i)
        prop "Level 2" $ \i k1 ->
            runProp2 (IM.member i) (EMM.member $ i :& K k1) k1
        prop "Level 3" $ \i k1 k2 ->
            runProp3 (IM.member i) (EMM.member $ i :& k1 :& K k2) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
            runProp4 (IM.member i) (EMM.member $ i :& k1 :& k2 :& K k3) k1 k2 k3

    describe "insert" $ do
        prop "Level 1" $ \i j ->
             runPropL (IM.insert i j) (EMM.insert (K i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insert i j) (EMM.insert (i :& K k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insert i j) (EMM.insert (i :& k1 :& K k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insert i j)
                           (EMM.insert (i :& k1 :& k2 :& K k3) j) k1 k2 k3

    describe "insertWith" $ do
        prop "Level 1" $ \i j ->
             runPropL (IM.insertWith (+) i j) $
                          (EMM.insertWith (+) (K i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (i :& K k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (i :& k1:& K k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insertWith (+) i j)
                           (EMM.insertWith (+) (i :& k1 :& k2 :& K k3) j) k1 k2 k3

    describe "insertWithKey" $ do
        let f a b c = a + b + c
        prop "Level 1" $ \i j ->
             runPropL (IM.insertWithKey f i j) $
                          (EMM.insertWithKey
                           (\(K k) -> f k)
                           (K i) j)
        prop "Level 2" $ \i j k1 ->
             runPropL2 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(k :& K _) -> f k)
                            (i :& K k1) j) k1
        prop "Level 3" $ \i j k1 k2 ->
             runPropL3 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(k :& _ :& K _) -> f k)
                            (i :& k1 :& K k2) j) k1 k2
        prop "Level 4" $ \i j k1 k2 k3 ->
             runPropL4 (IM.insertWithKey f i j)
                           (EMM.insertWithKey
                            (\(k :& _ :& _ :& K _) -> f k)
                            (i :& k1 :& k2 :& K k3) j) k1 k2 k3

    describe "delete" $ do
        prop "Level 1" $ \i ->
             runPropL (IM.delete i) (EMM.delete (K i))
        prop "Level 2" $ \i k1 ->
             runPropL2 (IM.delete i) (EMM.delete (i :& K k1)) k1
        prop "Level 3" $ \i k1 k2 ->
             runPropL3 (IM.delete i) (EMM.delete (i :& k1 :& K k2)) k1 k2
        prop "Level 4" $ \i k1 k2 k3 ->
             runPropL4 (IM.delete i)
                           (EMM.delete (i :& k1 :& k2 :& K k3)) k1 k2 k3

    describe "alter" $ do
        let f b n v = case v of
                          Just v' -> case b of
                                       True  -> Just v'
                                       False -> Nothing
                          Nothing -> case b of
                                       True -> Just n
                                       False -> Nothing
        prop "Level 1" $ \i b n ->
            runPropL (IM.alter (f b n) i) $
                     EMM.alter (f b n) (K i)
        prop "Level 2" $ \i b n k1 ->
            runPropL2 (IM.alter (f b n) i)
                         (EMM.alter (f b n) (i :& K k1)) k1
        prop "Level 3" $ \i b n k1 k2 ->
            runPropL3 (IM.alter (f b n) i)
                         (EMM.alter (f b n) (i :& k1 :& K k2)) k1 k2

    describe "foldr" $ do
        prop "Level 1" $
             runProp (IM.foldr (:) []) (EMM.foldr (:) [])
        prop "Level 2" $
             runProp2 (IM.foldr (:) []) (EMM.foldr (:) [])
        prop "Level 3" $
             runProp3 (IM.foldr (:) []) (EMM.foldr (:) [])

    describe "foldrWithKey" $ do
        let f a b c = [a + b] ++ c
        prop "Level 1" $
             runProp (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(K k) -> f k) [])
        prop "Level 2" $
             runProp2 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(k :& K _) -> f k) [])
        prop "Level 3" $
             runProp3 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(k :& _ :& K _) -> f k) [])
        prop "Level 3" $
             runProp4 (IM.foldrWithKey f []) (EMM.foldrWithKey
                         (\(k :& _ :& _ :& K _) -> f k) [])

    describe "map" $ do
        let f a = a + 1
        prop "Level 1" $
             runPropL (IM.map f) (EMM.map f)
        prop "Level 2" $
             runPropL2 (IM.map f) (EMM.map f)
        prop "Level 3" $
             runPropL3 (IM.map f) (EMM.map f)
        prop "Level 4" $
             runPropL4 (IM.map f) (EMM.map f)

    describe "mapWithKey" $ do
        let f k a = k + a
        prop "Level 1" $
             runPropL  (IM.mapWithKey f) (EMM.mapWithKey (f . unKey1))
        prop "Level 2" $
             runPropL2 (IM.mapWithKey f) (EMM.mapWithKey (f . unKey2))
        prop "Level 3" $
             runPropL3 (IM.mapWithKey f) (EMM.mapWithKey (f . unKey3))
        prop "Level 4" $
             runPropL4 (IM.mapWithKey f) (EMM.mapWithKey (f . unKey4))

    describe "findMin" $ do
        let go f (a, b) = (f a, b)
        prop "Level 1" $ \list ->
             (not $ L.null list) ==>
                 runProp (IM.findMin) (go unKey1 . EMM.findMin) list
        prop "Level 2" $ \k1 list ->
             (not $ L.null list) ==>
                 runProp2 (IM.findMin) (go unKey2 . EMM.findMin) k1 list
        prop "Level 3" $ \k1 k2 list ->
             (not $ L.null list) ==>
                 runProp3 (IM.findMin) (go unKey3 . EMM.findMin) k1 k2 list
        prop "Level 4" $ \k1 k2 k3 list ->
             (not $ L.null list) ==>
                 runProp4 (IM.findMin) (go unKey4 . EMM.findMin) k1 k2 k3 list

    describe "minViewWithKey" $ do
        let goe _ Nothing = Nothing
            goe f (Just ((k, v), emm)) = Just ((f k, v), EMM.toList emm)
            goi _ Nothing = Nothing
            goi f (Just ((k, v), im)) = Just ((k, v), f $ IM.toList im)
        prop "Level 1" $
             runProp (goi list2l1 . IM.minViewWithKey)
                         (goe unKey1 . EMM.minViewWithKey)
        prop "Level 2" $ \k1 ->
             runProp2 (goi (list2l2 k1) . IM.minViewWithKey)
                         (goe unKey2 . EMM.minViewWithKey) k1
        prop "Level 3" $ \k1 k2 ->
             runProp3 (goi (list2l3 k1 k2) . IM.minViewWithKey)
                         (goe unKey3 . EMM.minViewWithKey) k1 k2
        prop "Level 4" $ \k1 k2 k3 ->
             runProp4 (goi (list2l4 k1 k2 k3) . IM.minViewWithKey)
                         (goe unKey4 . EMM.minViewWithKey) k1 k2 k3

    describe "deleteFindMin" $ do
        let goe _ Nothing = Nothing
            goe f (Just ((k, v), emm)) = Just ((f k, v), EMM.toList emm)
            goi _ Nothing = Nothing
            goi f (Just ((k, v), im)) = Just ((k, v), f $ IM.toList im)
        prop "Level 1" $ \list ->
             (not $ L.null list) ==>
                 runProp (goi list2l1 . IM.minViewWithKey)
                            (goe unKey1 . EMM.minViewWithKey) list
        prop "Level 2" $ \k1 list ->
             (not $ L.null list) ==>
                 runProp2 (goi (list2l2 k1) . IM.minViewWithKey)
                             (goe unKey2 . EMM.minViewWithKey) k1 list
        prop "Level 3" $ \k1 k2 list ->
             (not $ L.null list) ==>
                 runProp3 (goi (list2l3 k1 k2) . IM.minViewWithKey)
                             (goe unKey3 . EMM.minViewWithKey) k1 k2 list
        prop "Level 4" $ \k1 k2 k3 list ->
             (not $ L.null list) ==>
                 runProp4 (goi (list2l4 k1 k2 k3) . IM.minViewWithKey)
                             (goe unKey4 . EMM.minViewWithKey) k1 k2 k3 list

    describe "union" $ do
        prop "Level 1" $
             runPropDuoL  IM.union EMM.union
        prop "Level 2" $
             runPropDuoL2 IM.union EMM.union
        prop "Level 3" $
             runPropDuoL3 IM.union EMM.union
        prop "Level 4" $
             runPropDuoL4 IM.union EMM.union

    describe "unionWith" $ do
        prop "Level 1" $
             runPropDuoL  (IM.unionWith (+)) (EMM.unionWith (+))
        prop "Level 2" $
             runPropDuoL2 (IM.unionWith (+)) (EMM.unionWith (+))
        prop "Level 3" $
             runPropDuoL3 (IM.unionWith (+)) (EMM.unionWith (+))
        prop "Level 4" $
             runPropDuoL4 (IM.unionWith (+)) (EMM.unionWith (+))

    describe "unionWithKey" $ do
        let f a b c = (a + b) * c
        prop "Level 1" $
             runPropDuoL (IM.unionWithKey f) (EMM.unionWithKey
                                              (\(K k) -> f k))
        prop "Level 2" $
             runPropDuoL2 (IM.unionWithKey f) (EMM.unionWithKey
                                              (\(k :& K _) -> f k))
        prop "Level 3" $
             runPropDuoL3 (IM.unionWithKey f) (EMM.unionWithKey
                                              (\(k :& _ :& K _) -> f k))
        prop "Level 4" $
             runPropDuoL4 (IM.unionWithKey f) (EMM.unionWithKey
                                              (\(k :& _ :& _ :& K _) -> f k))

    describe "intersectionWithKey" $ do
        let f a b c = (a + b) * c
        prop "Level 1" $
             runPropDuoL (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey
                                     (\(K k) a b ->  f k a b))
        prop "Level 2" $
             runPropDuoL2 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey
                                     (\(k :& K _) a b -> f k a b))
        prop "Level 3" $
             runPropDuoL3 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey
                                     (\(k :& _ :& K _) a b -> f k a b))
        prop "Level 4" $
             runPropDuoL4 (IM.intersectionWithKey f)
                             (EMM.intersectionWithKey
                                     (\(k :& _ :& _ :& K _) a b -> f k a b))

    describe "keys" $ do
        prop "Level 1" $
             runProp (IM.keys) (map (\(K k) -> k) . EMM.keys)
        prop "Level 2" $
             runProp2 (IM.keys) (map (\(k :& _) -> k) . EMM.keys)
        prop "Level 3" $
             runProp3 (IM.keys) (map (\(k :& _) -> k) . EMM.keys)
        prop "Level 4" $
             runProp4 (IM.keys) (map (\(k :& _) -> k) . EMM.keys)

    describe "elems" $ do
        prop "Level 1" $
             runProp (IM.elems) (EMM.elems)
        prop "Level 2" $
             runProp2 (IM.elems) (EMM.elems)
        prop "Level 3" $
             runProp3 (IM.elems) (EMM.elems)
        prop "Level 4" $
             runProp4 (IM.elems) (EMM.elems)

    describe "keysSet" $ do
        prop "Level 1" $
             runProp (set2l1 . IS.toList . IM.keysSet)
                         (EMS.toList . EMM.keysSet)
        prop "Level 2" $ \ s1 ->
             runProp2 (set2l2 s1 . IS.toList . IM.keysSet)
                         (EMS.toList . EMM.keysSet) s1
        prop "Level 3" $ \ s1 s2 ->
             runProp3 (set2l3 s1 s2 . IS.toList . IM.keysSet)
                         (EMS.toList . EMM.keysSet) s1 s2
        prop "Level 4" $ \ s1 s2 s3 ->
             runProp4 (set2l4 s1 s2 s3 . IS.toList . IM.keysSet)
                         (EMS.toList . EMM.keysSet) s1 s2 s3
