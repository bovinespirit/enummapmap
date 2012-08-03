{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Test.Hspec.HUnit ()
import           Test.Hspec.Monadic
import           Test.HUnit

import           Data.EnumMapMap.Base (EnumMapMap,
                                       Key1(..), Key2(..), Key3(..), Key4(..))
import qualified Data.EnumMapMap.Base as EMM

tens :: [Int]
tens = [1, 10, 100, 1000, 10000, 100000, 1000000]

odds :: [Int]
odds = [1, 3..1000]

fewOdds :: [Int]
fewOdds = [1, 3..6]

evens :: [Int]
evens = [2, 4..1000]

fewEvens :: [Int]
fewEvens = [2, 4..6]

alls :: [Int]
alls = [1, 2..1000]

fewAlls :: [Int]
fewAlls = [1, 2..6]

l1tens :: EnumMapMap (Key1 Int) Int
l1tens = EMM.fromList1 $ zip [1..7] tens
l2tens :: EnumMapMap (Key2 Int Int) Int
l2tens = EMM.fromList2 $ zip [1..2] $ repeat $ zip [1..7] tens
l3tens :: EnumMapMap (Key3 Int Int Int) Int
l3tens = EMM.fromList3 $ zip [1..2] $ repeat $ zip [1..2] $
         repeat $ zip [1..7] tens
l4tens :: EnumMapMap (Key4 Int Int Int Int) Int
l4tens = EMM.fromList4 $ zip [1..2] $ repeat $ zip [1..2] $ repeat $
         zip [1..2] $ repeat $ zip [1..7] tens
l1odds :: EnumMapMap (Key1 Int) Int
l1odds = EMM.fromList1 $ zip odds odds
l2odds :: EnumMapMap (Key2 Int Int) Int
l2odds = EMM.fromList2 $ zip odds $ repeat $ zip fewOdds fewOdds
l3odds :: EnumMapMap (Key3 Int Int Int) Int
l3odds = EMM.fromList3 $ zip odds $ repeat $ zip fewOdds $ repeat $ zip fewOdds fewOdds
l4odds :: EnumMapMap (Key4 Int Int Int Int) Int
l4odds = EMM.fromList4 $ zip odds $ repeat $
                      zip fewOdds $ repeat $ zip fewOdds $ repeat $ zip fewOdds fewOdds
l1evens :: EnumMapMap (Key1 Int) Int
l1evens = EMM.fromList1 $ zip evens evens

l1alls :: EnumMapMap (Key1 Int) Int
l1alls = EMM.fromList1 $ zip alls alls

main :: IO ()
main = hspecX $ do
         describe "empty" $ do
           it "creates an empty EnumMapMap" $
             (EMM.null $ EMM.empty)
           it "has a size of 0" $
             0 @=? (EMM.size $ EMM.empty)

         describe "fromList" $ do
           it "is the inverse of toList on 1 level" $
             (EMM.fromList1 $ EMM.toList1 l1odds) @?= l1odds
           it "is the inverse of toList on 2 levels" $
             (EMM.fromList2 $ EMM.toList2 l2odds) @?= l2odds
           it "is the inverse of toList on 3 levels" $
             (EMM.fromList3 $ EMM.toList3 l3odds) @?= l3odds
           it "is the inverse of toList on 4 levels" $
             (EMM.fromList4 $ EMM.toList4 l4odds) @?= l4odds

         describe "insert" $ do
           it "" True
           describe "Level 1" $ do
             it "creates a value in an empty EMM" $
               EMM.insert (Key1 1) 1 EMM.empty @?=
                      (EMM.fromList1 [(1, 1)]
                           :: EnumMapMap (Key1 Int) Int)
             it "adds another value to an EMM" $
               let
                   emm :: EnumMapMap (Key1 Int) Int
                   emm = EMM.fromList1 [(2, 2)] in
               EMM.insert (Key1 1) 1 emm @?=
               EMM.fromList1 [(1, 1), (2, 2)]
             it "overwrites a value with the same key in an EMM" $
               let emm :: EnumMapMap (Key1 Int) Int
                   emm = EMM.fromList1 [(1, 1), (2, 2)] in
               EMM.insert (Key1 1) 3 emm @?=
                  EMM.fromList1 [(1, 3), (2, 2)]

           describe "Level 2" $ do
             it "creates a value in an empty EMM" $
               EMM.insert (Key2 1 1) 1 EMM.empty @?=
                             (EMM.fromList2 [(1, [(1, 1)])]
                                  :: EnumMapMap (Key2 Int Int) Int)
             it "adds another value to an EMM on level 1" $
               let
                   emm :: EnumMapMap (Key2 Int Int) Int
                   emm = EMM.fromList2 [(1, [(2, 2)])]
               in
                 EMM.insert (Key2 1 1) 1 emm @?=
                 EMM.fromList2 [(1, [(1, 1), (2, 2)])]
             it "adds another value to an EMM on level 2" $
               let
                   emm :: EnumMapMap (Key2 Int Int) Int
                   emm = EMM.fromList2 [(1, [(1, 1)])]
               in
                 EMM.insert (Key2 2 2) 2 emm @?=
                 EMM.fromList2 [(1, [(1, 1)]), (2, [(2, 2)])]

         describe "insertWithKey" $ do
           let undef = undefined -- fail if this is called
           it "" True
           describe "Level 1" $ do
             it "creates a value in an empty EMM" $
               EMM.insertWithKey undef (Key1 1) 1 EMM.empty @?=
                      (EMM.fromList1 [(1, 1)]
                           :: EnumMapMap (Key1 Int) Int)
             it "adds another value to an EMM" $
               let
                   emm :: EnumMapMap (Key1 Int) Int
                   emm = EMM.fromList1 [(2, 2)] in
               EMM.insertWithKey undef (Key1 1) 1 emm @?=
               EMM.fromList1 [(1, 1), (2, 2)]
             it "applies the function when overwriting" $
               let emm :: EnumMapMap (Key1 Int) Int
                   emm = EMM.fromList1 [(1, 1), (2, 4)]
                   f (Key1 k1) o n = k1 * (o + n)
               in
                 EMM.insertWithKey f (Key1 2) 3 emm @?=
                    EMM.fromList1 [(1, 1), (2, 14)]

           describe "Level 2" $ do
             it "creates a value in an empty EMM" $
               EMM.insertWithKey undef (Key2 1 1) 1 EMM.empty @?=
                      (EMM.fromList2 [(1, [(1, 1)])]
                           :: EnumMapMap (Key2 Int Int) Int)
             it "adds another value to an EMM on level 1" $
               let
                   emm :: EnumMapMap (Key2 Int Int) Int
                   emm = EMM.fromList2 [(1, [(2, 2)])]
               in
                 EMM.insertWithKey undef (Key2 1 1) 1 emm @?=
                    EMM.fromList2 [(1, [(1, 1), (2, 2)])]
             it "adds another value to an EMM on level 2" $
               let
                   emm :: EnumMapMap (Key2 Int Int) Int
                   emm = EMM.fromList2 [(1, [(1, 1)])]
               in
                 EMM.insertWithKey undef (Key2 2 2) 2 emm @?=
                    EMM.fromList2 [(1, [(1, 1)]), (2, [(2, 2)])]
             it "applies the function when overwriting" $
               let emm :: EnumMapMap (Key2 Int Int) Int
                   emm = EMM.fromList2 [(2, [(3, 1), (4, 5)])]
                   f (Key2 k1 k2) o n = (k1 + k2) * (o + n)
               in
               EMM.insertWithKey f (Key2 2 4) 3 emm @?=
                  EMM.fromList2 [(2, [(3, 1), (4, 48)])]


         describe "foldrWithKey" $ do
           it "" True
           describe "Level 1" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey1 (\_ -> (+)) 0 l1tens @?= 1111111
             it "folds across all keys in an EnumMapMap" $
               EMM.foldrWithKey (\(Key1 k1) _ -> (+) k1) 0 l1tens @?= 28
             it "folds across all keys in one level of an EnumMapMap" $
               EMM.foldrWithKey (\(Key1 k1) _ -> (+) k1) 0 l4tens @?= 3
           describe "Level 2" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey2 (\_ -> (+)) 0 l2tens @?= 2222222
             it "folds across all keys in an EnumMapMap" $
               EMM.foldrWithKey
                      (\(Key2 k1 k2) _ -> (+) (k1 * k2)) 0 l2tens @?= 84
           describe "Level 3" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey3 (\_ -> (+)) 0 l3tens @?= 4444444
             it "folds across all keys in an EnumMapMap" $
               EMM.foldrWithKey
                      (\(Key3 k1 k2 k3) _ -> (+) (k1 * k2 * k3)) 0 l3tens @?=
                      252
           describe "Level 4" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey4 (\_ -> (+)) 0 l4tens @?= 8888888
             it "folds across all keys in an EnumMapMap" $
               EMM.foldrWithKey
                      (\(Key4 k1 k2 k3 k4) _ -> (+) (k1 * k2 * k3 * k4)) 0 l4tens
                      @?= 756

         describe "union" $ do
           it "" True
           describe "Level 1" $ do
             it "includes every key from each EnumMapMap" $
               (EMM.union1 l1odds l1evens) @?= l1alls
