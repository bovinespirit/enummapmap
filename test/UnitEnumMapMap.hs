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

evens :: [Int]
evens = [2, 4..1000]

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
l2odds = EMM.fromList2 $ zip odds $ repeat $ zip tens tens
l3odds :: EnumMapMap (Key3 Int Int Int) Int
l3odds = EMM.fromList3 $ zip odds $ repeat $ zip tens $ repeat $ zip tens tens
l4odds :: EnumMapMap (Key4 Int Int Int Int) Int
l4odds = EMM.fromList4 $ zip odds $ repeat $
                      zip tens $ repeat $ zip tens $ repeat $ zip tens tens

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

         describe "foldrWithKey" $ do
           it "" True
           describe "Level 1" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey1 (\_ -> (+)) 0 l1tens @?= 1111111
           describe "Level 2" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey2 (\_ -> (+)) 0 l2tens @?= 2222222
           describe "Level 3" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey3 (\_ -> (+)) 0 l3tens @?= 4444444
           describe "Level 4" $ do
             it "folds across all values in an EnumMapMap" $
               EMM.foldrWithKey4 (\_ -> (+)) 0 l4tens @?= 8888888
