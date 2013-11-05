{-# LANGUAGE
  CPP,
  DeriveDataTypeable,
  GeneralizedNewtypeDeriving,
  TypeOperators #-}

import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.QuickCheck (prop)
import           Test.Hspec
import           Test.HUnit

import qualified Data.List as List
import           Data.Typeable

import           Data.EnumMapSet (EnumMapSet, (:&)(..), S(..))
import qualified Data.EnumMapSet as EMS

newtype ID1 = ID1 Int
    deriving (Show, Enum, Eq, Num, Typeable)
newtype ID2 = ID2 Int
    deriving (Show, Enum, Eq, Num, Typeable)

type TestKey1 = S ID1
type TestEms1 = EnumMapSet TestKey1
type TestKey2 = ID2 :& S ID1
type TestEms2 = EnumMapSet TestKey2

tens :: [Int]
tens = [1, 10, 100, 1000, 10000, 100000, 1000000]

l1IDtens :: TestEms1
l1IDtens = EMS.fromList $ map (\k -> S $ ID1 k) tens

l1tens :: EnumMapSet (S Int)
l1tens = EMS.fromList $ map (\k -> S k) tens

l2tens :: TestEms2
l2tens = EMS.fromList $ do
           k1 <- [1, 5, 10]
           k2 <- tens
           return $ (ID2 k2) :& (S $ ID1 k1)

main :: IO ()
main =
  hspec $ do
    describe "all" $ do
      let f :: S Int -> Bool
          f (S s) = s > 0
      it "returns True for an empty EnumMapSet" $
         EMS.all (const False) (EMS.empty :: EnumMapSet (Int :& S Int))
                `shouldBe` True
      it "returns False when given all False" $
         EMS.all (const False) (EMS.fromList [S 1, S (2 :: Int)])
                `shouldBe` False
      it "returns False when given one False" $
         EMS.all f (EMS.fromList [S 5, S 2, S (-1),S 1000])
                `shouldBe` False
      let prop_list :: [Int] -> Bool
          prop_list list =
              let list' = map S list in
              EMS.all f (EMS.fromList list') == List.all f list'
      prop "is equivalent to List.all" prop_list


    describe "Typeable Instance" $ do
      it "TypeOf is unique when ID types differ" $
         ((typeOf l1IDtens) == (typeOf l1tens)) @?= False
      it "TypeOf is unique when different levels" $
         ((typeOf l2tens) == (typeOf l1tens)) @?= False
