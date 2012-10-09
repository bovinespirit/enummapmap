{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Monad (liftM, liftM2)
import           Test.Hspec.HUnit ()
import           Test.Hspec.Monadic
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit
import           Test.QuickCheck (Arbitrary, arbitrary, shrink)

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

l1tens :: EnumMapMap (K Int) Int
l1tens = EMM.fromList $ map (\(k, v) -> (K k, v)) $ zip [1..7] tens
l2tens :: EnumMapMap (Int :& K Int) Int
l2tens = EMM.fromList $ zip (do
                              k1 <- [1, 2]
                              k2 <- [1..7]
                              return $ k1 :& K k2) $ cycle tens

l1odds :: EnumMapMap (K Int) Int
l1odds = EMM.fromList $ map (\(k, v) -> (K k, v)) $ zip odds odds
l2odds :: EnumMapMap (Int :& K Int) Int
l2odds = EMM.fromList $ zip (do
                              k1 <- fewOdds
                              k2 <- fewOdds
                              return $ k1 :& K k2) $ cycle odds
l1evens :: EnumMapMap (K Int) Int
l1evens = EMM.fromList $ map (\(k, v) -> (K k, v)) $ zip evens evens

l1alls :: EnumMapMap (K Int) Int
l1alls = EMM.fromList $ zip (map K alls) alls

main :: IO ()
main =
  hspecX $ do
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

    describe "insert" $ do
      describe "Level 1" $ do
        it "creates a value in an empty EMM" $
           EMM.insert (K 1) 1 EMM.empty @?=
           (EMM.fromList [(K 1, 1)]
                           :: EnumMapMap (K Int) Int)
        it "adds another value to an EMM" $
           let
               emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 2, 2)] in
           EMM.insert (K 1) 1 emm @?=
              EMM.fromList [(K 1, 1), (K 2, 2)]
        it "overwrites a value with the same key in an EMM" $
           let emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 1, 1), (K 2, 2)] in
           EMM.insert (K 1) 3 emm @?=
              EMM.fromList [(K 1, 3), (K 2, 2)]

        describe "Level 2" $ do
          it "creates a value in an empty EMM" $
             EMM.insert (1 :& K 1) 1 EMM.empty @?=
                             (EMM.fromList [(1 :& K 1, 1)]
                                  :: EnumMapMap (Int :& K Int) Int)
          it "adds another value to an EMM on level 1" $
             let
                 emm :: EnumMapMap (Int :& K Int) Int
                 emm = EMM.fromList [(1 :& K 2, 2)]
             in
               EMM.insert (1 :& K 1) 1 emm @?=
               EMM.fromList [(1 :& K 1, 1), (1 :& K 2, 2)]
          it "adds another value to an EMM on level 2" $
             let
                 emm :: EnumMapMap (Int :& K Int) Int
                 emm = EMM.fromList [(1 :& K 1, 1)]
             in
               EMM.insert (2 :& K 2) 2 emm @?=
               EMM.fromList [(1 :& K 1, 1), (2 :& K 2, 2)]

    describe "insertWithKey" $ do
      let undef = undefined -- fail if this is called
      describe "Level 1" $ do
        it "creates a value in an empty EMM" $
           EMM.insertWithKey undef (K 1) 1 EMM.empty @?=
                  (EMM.fromList [(K 1, 1)]
                       :: EnumMapMap (K Int) Int)
        it "adds another value to an EMM" $
           let
               emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 2, 2)] in
           EMM.insertWithKey undef (K 1) 1 emm @?=
              EMM.fromList [(K 1, 1), (K 2, 2)]
        it "applies the function when overwriting" $
           let emm :: EnumMapMap (K Int) Int
               emm = EMM.fromList [(K 1, 1), (K 2, 4)]
               f (K key1) o n = key1 * (o + n)
           in
             EMM.insertWithKey f (K 2) 3 emm @?=
                EMM.fromList [(K 1, 1), (K 2, 14)]

      describe "Level 2" $ do
        it "creates a value in an empty EMM" $
           EMM.insertWithKey undef (1 :& K 1) 1 EMM.empty @?=
                  (EMM.fromList [(1 :& K 1, 1)]
                           :: EnumMapMap (Int :& K Int) Int)
        it "adds another value to an EMM on level 1" $
           let
               emm :: EnumMapMap (Int :& K Int) Int
               emm = EMM.fromList [(1 :& K 2, 2)]
           in
             EMM.insertWithKey undef (1 :& K 1) 1 emm @?=
                EMM.fromList [(1 :& K 1, 1), (1 :& K 2, 2)]
        it "adds another value to an EMM on level 2" $
           let
               emm :: EnumMapMap (Int :& K Int) Int
               emm = EMM.fromList [(1 :& K 1, 1)]
           in
             EMM.insertWithKey undef (2 :& K 2) 2 emm @?=
                EMM.fromList [(1 :& K 1, 1), (2 :& K 2, 2)]
        it "applies the function when overwriting" $
           let emm :: EnumMapMap (Int :& K Int) Int
               emm = EMM.fromList [(2 :& K 3, 1), (2 :& K 4, 5)]
               f (k1 :& K k2) o n = (k1 + k2) * (o + n)
           in
             EMM.insertWithKey f (2 :& K 4) 3 emm @?=
                EMM.fromList [(2 :& K 3, 1), (2 :& K 4, 48)]


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

      describe "union" $ do
        describe "Level 1" $ do
          it "includes every key from each EnumMapMap" $
               (EMM.union l1odds l1evens) @?= l1alls

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

