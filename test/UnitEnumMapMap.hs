{-# LANGUAGE
  CPP,
  DeriveDataTypeable,
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  ScopedTypeVariables,
  TypeFamilies,
  TypeOperators,
  UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Exception
import           Control.Monad (liftM, liftM2)
import qualified Data.Foldable as Foldable
import           Data.SafeCopy
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Data.Semigroup
import           Data.Typeable

import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit
import           Test.QuickCheck (Arbitrary, arbitrary, shrink, listOf)

import qualified Data.EnumMapSet as EMS

#ifdef LAZY
import           Data.EnumMapMap.Lazy(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Lazy as EMM
#else
import           Data.EnumMapMap.Strict(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Strict as EMM
#endif

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :& b) where
    arbitrary = liftM2 (:&) arbitrary arbitrary
    shrink (x :& y) =    [ x' :& y | x' <- shrink x ]
                      ++ [ x :& y' | y' <- shrink y ]

instance (Arbitrary a) => Arbitrary (K a) where
    arbitrary = liftM K arbitrary

instance (Arbitrary k, Arbitrary v,
          EMM.IsKey k, EMM.SubKey k k v, EMM.Result k k v ~ v) =>
    Arbitrary (EnumMapMap k v) where
        arbitrary = fmap EMM.fromList $ listOf $ do
                                   key <- arbitrary
                                   val <- arbitrary
                                   return (key, val)

newtype ID1 = ID1 Int
    deriving (Show, Enum, Arbitrary, Eq, Num, Typeable)
newtype ID2 = ID2 Int
    deriving (Show, Enum, Arbitrary, Eq, Num, Typeable)
newtype ID3 = ID3 Int
    deriving (Show, Enum, Arbitrary, Eq, Num, Typeable)

type TestKey1 = K ID1
type TestEmm1 = EnumMapMap TestKey1 Int
type TestKey2 = ID2 :& K ID1
type TestEmm2 = EnumMapMap TestKey2 Int
type TestEmm2B = EnumMapMap TestKey2 Bool
type TestKey3 = ID3 :& ID2 :& K ID1
type TestEmm3 = EnumMapMap TestKey3 Int

type I = K Int

-- Functions that are part of 'SubKey' class can't cope with @K 1@ because GHC
-- doesn't know it's also an 'Int'.
k :: Int -> K Int
k = K

s :: Int -> EMS.S Int
s = EMS.S

tens :: [Int]
tens = [1, 10, 100, 1000, 10000, 100000, 1000000]

odds :: [Int]
odds = [1, 3..1000]

fewOdds :: [Int]
fewOdds = [1, 3..6]

evens :: [Int]
evens = [2, 4..1000]

alls :: [Int]
alls = [1, 2..1000]

l1tens :: EnumMapMap I Int
l1tens = EMM.fromList $ map (\(key, v) -> (K key, v)) $ zip [1..7] tens
l1IDtens :: TestEmm1
l1IDtens = EMM.fromList $ map (\(key, v) -> (K $ ID1 key, v)) $ zip [1..7] tens
l2tens :: EnumMapMap (Int :& I) Int
l2tens = EMM.fromList $ zip (do
                              k1 <- [1, 2]
                              k2 <- [1..7]
                              return $ k1 :& K k2) $ cycle tens

l1odds :: EnumMapMap (K Int) Int
l1odds = EMM.fromList $ map (\(key, v) -> (K key, v)) $ zip odds odds
l1fewOdds :: EnumMapMap (K Int) Int
l1fewOdds = EMM.fromList $ map (\(key, v) -> (K key, v)) $ zip fewOdds fewOdds
l2odds :: EnumMapMap (Int :& K Int) Int
l2odds = EMM.fromList $ zip (do
                              k1 <- fewOdds
                              k2 <- fewOdds
                              return $ k1 :& K k2) $ cycle odds
l1evens :: EnumMapMap (K Int) Int
l1evens = EMM.fromList $ map (\(key, v) -> (K key, v)) $ zip evens evens

l1alls :: EnumMapMap (K Int) Int
l1alls = EMM.fromList $ zip (map K alls) alls

checkSubs :: (TestEmm3 -> TestEmm3 -> TestEmm3)
          -> [(TestKey3, Int)]
          -> [(TestKey3, Int)]
          -> Bool
checkSubs f l1 l2 =
    False == (EMM.emptySubTrees $ f emm1 emm2)
        where
          emm1 = EMM.fromList l1
          emm2 = EMM.fromList l2

checkSubs1 :: (TestEmm3 -> TestEmm3)
           -> [(TestKey3, Int)]
           -> Bool
checkSubs1 f l1 =
    False == (EMM.emptySubTrees $ f emm1)
        where
          emm1 = EMM.fromList l1

main :: IO ()
main =
  hspec $ do
    describe "empty" $ do
      it "creates an empty EnumMapMap" $
           (EMM.null $ (EMM.empty :: EnumMapMap (Int :& Int :& K Int) Bool))
      it "has a size of 0" $
           0 @=? (EMM.size $ (EMM.empty :: EnumMapMap (Int :& K Int) Bool))

    describe "fromList" $ do
      it "is the inverse of toList on 1 level" $
           (EMM.fromList $ EMM.toList l1odds) @?= l1odds
      it "is the inverse of toList on 2 levels" $
           (EMM.fromList $ EMM.toList l2odds) @?= l2odds

    describe "lookup" $ do
      let emm3 :: TestEmm3
          emm3 = EMM.fromList [(ID3 1 :& ID2 2 :& (K $ ID1 3), 4)]
          key3 = ID3 1 :& ID2 2 :& (K $ ID1 3)
      describe "looks up a subtree" $ do
         let emm2 :: EnumMapMap (Int :& K Int) Int
             emm2 = EMM.fromList [(1 :& k 2, 5)]
             key1 :: K ID3
             key1 = K $ ID3 1
             key2 :: ID3 :& K ID2
             key2 = ID3 1 :& (K $ ID2 2)
         it "First level of level 2" $
            (EMM.lookup (K 1) emm2) @?= (Just $ EMM.fromList [(K 2, 5)])
         it "1 level of level 3" $
            (EMM.lookup key1 emm3) @?= (Just $
                                     EMM.fromList [(ID2 2 :& (K $ ID1 3), 4)])
         it "2 levels of level 3" $
            (EMM.lookup key2 emm3) @?= (Just $ EMM.fromList [(K $ ID1 3, 4)])
      it "looks up a value" $
         (EMM.lookup key3 emm3) @?= Just 4

    describe "singleton" $ do
      let emm2 :: EnumMapMap (ID1 :& K ID2) String
          emm2 = EMM.fromList [(ID1 1 :& (K $ ID2 2), "a")]
      it "creates an EnumMapMap with one value" $
         (EMM.singleton (ID1 1 :& (K $ ID2 2)) "a") @?= emm2
      it "creates an EnumMapMap with a sub EnumMapMap" $
         (EMM.singleton (K $ ID1 1) $ EMM.singleton (K $ ID2 2) "a") @?= emm2

    describe "insert" $ do
      describe "Level 1" $ do
        it "creates a value in an empty EMM" $
           EMM.insert (k 1) 1 EMM.empty @?=
           (EMM.fromList [(k 1, 1)]
                           :: EnumMapMap I Int)
        it "adds another value to an EMM" $
           let
               emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(k 2, 2)] in
           EMM.insert (k 1) 1 emm @?=
              EMM.fromList [(k 1, 1), (k 2, 2)]
        it "overwrites a value with the same key in an EMM" $
           let emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 1, 1), (K 2, 2)] in
           EMM.insert (k 1) 3 emm @?=
              EMM.fromList [(K 1, 3), (K 2, 2)]

        describe "Level 2" $ do
          it "creates a value in an empty EMM" $
             EMM.insert ((1 :: Int) :& k 1) 1 EMM.empty @?=
                             (EMM.fromList [(1 :& K 1, 1)]
                                  :: EnumMapMap (Int :& K Int) Int)
          it "adds another value to an EMM on level 1" $
             let
                 emm :: EnumMapMap (Int :& K Int) Int
                 emm = EMM.fromList [(1 :& K 2, 2)]
             in
               EMM.insert ((1 :: Int) :& k 1) 1 emm @?=
               EMM.fromList [(1 :& K 1, 1), (1 :& K 2, 2)]
          it "adds another value to an EMM on level 2" $
             let
                 emm :: EnumMapMap (Int :& K Int) Int
                 emm = EMM.fromList [(1 :& K 1, 1)]
             in
               EMM.insert ((2 :: Int) :& k 2) 2 emm @?=
               EMM.fromList [(1 :& K 1, 1), (2 :& K 2, 2)]

        describe "Subtrees" $ do
          let emm2 :: TestEmm2
              emm2 = EMM.fromList [(ID2 2 :& (K $ ID1 3), 4)]
              emm1 :: TestEmm1
              emm1 = EMM.fromList [(K $ ID1 4, 12)]
          it "inserts a L1 into an empty L3 EMM" $
             EMM.insert (ID3 2 :& (K $ ID2 3)) emm1 EMM.empty @?=
                EMM.fromList [(ID3 2 :& ID2 3 :& (K $ ID1 4), 12)]
          it "inserts a L2 into an empty L3 EMM" $
             EMM.insert (K $ ID3 1) emm2 EMM.empty @?=
                EMM.fromList [(ID3 1 :& ID2 2 :& (K $ ID1 3), 4)]

    describe "insertWithKey" $ do
      let undef = undefined -- fail if this is called
      describe "Level 1" $ do
        it "creates a value in an empty EMM" $
           EMM.insertWithKey undef (k 1) 1 EMM.empty @?=
                  (EMM.fromList [(k 1, 1)]
                       :: EnumMapMap (K Int) Int)
        it "adds another value to an EMM" $
           let
               emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 2, 2)] in
           EMM.insertWithKey undef (k 1) 1 emm @?=
              EMM.fromList [(k 1, 1), (k 2, 2)]
        it "applies the function when overwriting" $
           let emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(k 1, 1), (k 2, 4)]
               f (K key1) o n = key1 * (o + n)
           in
             EMM.insertWithKey f (k 2) 3 emm @?=
                EMM.fromList [(k 1, 1), (k 2, 14)]

      describe "Level 2" $ do
        it "creates a value in an empty EMM" $
           EMM.insertWithKey undef (ID2 1 :& k 1) 1 EMM.empty @?=
                  (EMM.fromList [(ID2 1 :& k 1, 1)]
                           :: EnumMapMap (ID2 :& K Int) Int)
        it "adds another value to an EMM on level 1" $
           let
               emm :: EnumMapMap (ID2 :& K Int) Int
               emm = EMM.fromList [(ID2 1 :& k 2, 2)]
           in
             EMM.insertWithKey undef (ID2 1 :& k 1) 1 emm @?=
                EMM.fromList [(ID2 1 :& K 1, 1), (ID2 1 :& K 2, 2)]
        it "adds another value to an EMM on level 2" $
           let
               emm :: EnumMapMap (ID2 :& K Int) Int
               emm = EMM.fromList [(ID2 1 :& k 1, 1)]
           in
             EMM.insertWithKey undef (ID2 2 :& k 2) 2 emm @?=
                EMM.fromList [(ID2 1 :& K 1, 1), (ID2 2 :& K 2, 2)]
        it "applies the function when overwriting" $
           let emm :: EnumMapMap (Int :& K Int) Int
               emm = EMM.fromList [((2 :: Int) :& K 3, 1), ((2 :: Int) :& K 4, 5)]
               f (k1 :& K k2) o n = (k1 + k2) * (o + n)
           in
             EMM.insertWithKey f (2 :& k 4) 3 emm @?=
                EMM.fromList [((2 :: Int) :& K 3, 1), ((2 :: Int) :& K 4, 48)]

    describe "delete" $ do
      describe "leaves no empty subtrees" $ do
        prop "Full key" $ \(key :: ID3 :& ID2 :& K ID1) l ->
          not $ EMM.emptySubTrees $ EMM.delete key $ (EMM.fromList l :: TestEmm3)
        prop "2 dimensional key" $ \(key :: ID3 :& K ID2) l ->
          not $ EMM.emptySubTrees $ EMM.delete key $ (EMM.fromList l :: TestEmm3)
        prop "1 dimensional key" $ \(key :: K ID3) l ->
          not $ EMM.emptySubTrees $ EMM.delete key $ (EMM.fromList l :: TestEmm3)

    describe "alter" $ do
      let f b1 b2 n v = case v of
                          Nothing -> if b1 then Just n else Nothing
                          Just v' -> case b1 of
                                       True  -> Just $ if b2 then v' else n
                                       False -> Nothing
      prop "leaves no empty subtrees" $ \key l b1 b2 n ->
          not $ EMM.emptySubTrees $ EMM.alter (f b1 b2 n) key $
                  (EMM.fromList l :: TestEmm3)

    describe "foldrWithKey" $ do
      describe "Level 1" $ do
        it "folds across all values in an EnumMapMap" $
           EMM.foldrWithKey (\_ -> (+)) 0 l1tens @?= 1111111
        it "folds across all keys in an EnumMapMap" $
           EMM.foldrWithKey (\(K k1) _ -> (+) k1) 0 l1tens @?= 28
      describe "Level 2" $ do
        it "folds across all values in an EnumMapMap" $
           EMM.foldrWithKey (\_ -> (+)) 0 l2tens @?= 2222222
        it "folds across all keys in an EnumMapMap" $
           EMM.foldrWithKey
                      (\(k1 :& K k2) _ -> (+) (k1 * k2)) 0 l2tens @?= 84

    describe "mapMaybe" $ do
      let f v
            | v > 2 = Just $ v
            | otherwise = Nothing
      prop "No empty subtrees" $
           checkSubs1 (EMM.mapMaybe f)

    describe "mapMaybeWithKey" $ do
      let f _ v
            | v > 2 = Just $ v
            | otherwise = Nothing
      prop "No empty subtrees" $
           checkSubs1 (EMM.mapMaybeWithKey f)

    describe "union" $ do
        describe "Level 1" $ do
          it "includes every key from each EnumMapMap" $
               (EMM.union l1odds l1evens) @?= l1alls
        -- Just in case...
        prop "Leaves no empty subtrees" $ checkSubs EMM.union

    describe "difference" $ do
        prop "Leaves no empty subtrees" $ checkSubs EMM.difference

    describe "differenceWithKey" $ do
        let f (k1 :& k2 :& K k3) v1 v2 =
                Just $ v1 + v2 + (fromEnum k1) + (fromEnum k2) + (fromEnum k3)
        prop "Leaves no empty subtrees" $ checkSubs (EMM.differenceWithKey f)

    describe "intersection" $ do
        prop "Leaves no empty subtrees" $ checkSubs EMM.intersection

    describe "intersectionWithKey" $ do
        let f (k1 :& k2 :& K k3) v1 v2 =
                v1 + v2 + (fromEnum k1) + (fromEnum k2) + (fromEnum k3)
        prop "Leaves no empty subtrees" $ checkSubs (EMM.intersectionWithKey f)

    describe "joinKey $ splitKey z t == t" $ do
        let go21 :: [(Int :& K Int, Int)] -> Bool
            go21 l = emm == (EMM.joinKey $ EMM.splitKey EMM.d1 emm)
                where emm = EMM.fromList l
        prop "Level 2, depth = 1" go21

        let go31 :: [(Int :& Int :& K Int, Int)] -> Bool
            go31 l = emm == (EMM.joinKey $ EMM.splitKey EMM.d1 emm)
                where emm = EMM.fromList l
        prop "Level 3, depth = 1" go31

        let go32 :: [(Int :& Int :& K Int, Int)] -> Bool
            go32 l = emm == (EMM.joinKey $ EMM.splitKey EMM.d2 emm)
                where emm = EMM.fromList l
        prop "Level 3, depth = 2" go32

    describe "keysSet" $ do
        describe "produces same result as keys" $ do
          let gol1 :: [(K Int, Int)] -> Bool
              gol1 list = EMM.keys emm == (map EMM.toK $ EMS.toList $ EMM.keysSet emm)
                  where
                    emm = EMM.fromList list
          prop "Level 1" gol1

    describe "intersectSet" $ do
        it "leaves correct values" $
           (EMM.intersectSet l1odds $ EMS.fromList [s 1, s 2, s 3])
           @?= EMM.fromList [(k 1, 1), (k 3, 3)]
        it "leaves correct subtree" $
           (EMM.intersectSet l2odds $ EMS.fromList [s 1])
           @?= EMM.fromList [(1 :& k 1, 1), (1 :& k 3, 3), (1 :& k 5, 5)]
        -- TODO: check for empty subtrees

    describe "differenceSet" $ do
        it "works correctly" $
           (EMM.differenceSet l1fewOdds $ EMS.fromList [s 3, s 4, s 5])
           @?= EMM.fromList [(k 1, 1)]
        it "leaves correct subtree" $
           (EMM.differenceSet l2odds $ EMS.fromList [s 3, s 4, s 5])
           @?= EMM.fromList [(1 :& k 1, 1), (1 :& k 3, 3), (1 :& k 5, 5)]

    describe "findMin" $ do
        it "throws an error when it is passed an empty EnumMapMap" $ do
           evaluate (EMM.findMin (EMM.empty :: EnumMapMap (K Int) Int))
                        `shouldThrow` anyErrorCall

    describe "deleteFindMin" $ do
        it "throws an error when it is passed an empty EnumMapMap" $ do
           evaluate (EMM.deleteFindMin (EMM.empty :: EnumMapMap (K Int) Int))
                        `shouldThrow` anyErrorCall

    describe "Monoid/Semigroup instances" $ do
        let uvsm :: TestEmm3 -> TestEmm3 -> Bool
            uvsm emm1 emm2 =
                ((EMM.map Sum emm1) <> (EMM.map Sum emm2)) ==
                ( EMM.map Sum $ EMM.unionWith (+) emm1 emm2)
        prop "mappend works like unionWith mappend" uvsm
        let lvsi :: TestEmm3 -> TestEmm3 -> Bool
            lvsi emm1 emm2
                = ((EMM.map First emm1) <> (EMM.map First emm2)) ==
                  (EMM.map First $ EMM.union emm1 emm2)
        prop "(<>) First works like union" lvsi
        let bvsu :: [TestEmm2B] -> Bool
            bvsu emms =
                (mconcat $ map (EMM.map All) emms) ==
                (EMM.map All $ EMM.unionsWith (&&) emms)
        prop "unionsWith (&&) works like mconcat All" bvsu

    describe "Foldable instance" $ do
      describe "Foldable.all" $ do
          it "Level 1 true" $
             True @=? Foldable.all (> 0) l1tens
          it "Level 1 false" $
             False @=? Foldable.all (> 1) l1tens
          it "Level 1 true with newtype key" $
             True @=? Foldable.all (> 0) l1IDtens
          it "Level 1 false with newtype key" $
             False @=? Foldable.all (> 1) l1IDtens
      describe "Foldable.any" $ do
          it "Level 1 true" $
             False @=? Foldable.any (< 1) l1tens
          it "Level 1 false" $
             True @=? Foldable.any (< 2) l1tens

    describe "Typeable Instance" $ do
      it "TypeOf is unique when ID types differ" $
         ((typeOf l1IDtens) == (typeOf l1tens)) @?= False
      it "TypeOf is unique when different levels" $
         ((typeOf l2tens) == (typeOf l1tens)) @?= False

    describe "SafeCopy instance" $ do
      let testEq :: TestEmm3 -> Bool
          testEq emm = op == Right emm
              where
                op = runGet safeGet $ runPut $ safePut emm
      prop "Leaves data intact" testEq
