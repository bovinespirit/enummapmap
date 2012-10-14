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

list2l1 :: [Int] -> [K Int]
list2l1 = map (\k -> K k)

runProp :: Eq t =>
           (IS.IntSet -> t)
        -> (TestSet1 -> t)
        -> [Int]
        -> Bool
runProp f g list =
    (f $ IS.fromList list) == (g $ EMS.fromList $ list2l1 list)

runPropL :: (IS.IntSet -> IS.IntSet)
         -> (TestSet1 -> TestSet1)
         -> [Int]
         -> Bool
runPropL f g
    = runProp (list2l1 . IS.toList . f) (EMS.toList . g)

main :: IO ()
main = hspecX $ do
    describe "toList fromList" $ do
      prop "works" $ runPropL id id
