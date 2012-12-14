{-# LANGUAGE CPP, TypeOperators #-}

import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.QuickCheck (prop)
import           Test.Hspec.Monadic

import           Data.EnumMapSet (EnumMapSet, (:&)(..), S(..))
import qualified Data.EnumMapSet as EMS
import qualified Data.List as List

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

