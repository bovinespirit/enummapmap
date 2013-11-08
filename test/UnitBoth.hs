{-# LANGUAGE
  CPP,
  DeriveDataTypeable,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  OverlappingInstances,
  TypeFamilies,
  TypeOperators,
  TypeSynonymInstances,
  UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Test.Hspec.QuickCheck (prop)
import           Test.Hspec
import           Test.QuickCheck (Arbitrary, arbitrary, shrink, listOf)

import           Control.Monad (liftM, liftM2)
import           Data.SafeCopy
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Data.Typeable

#ifdef LAZY
import           Data.EnumMapMap.Lazy(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Lazy as EMM
#else
import           Data.EnumMapMap.Strict(EnumMapMap, (:&)(..), K(..))
import qualified Data.EnumMapMap.Strict as EMM
#endif

import           Data.EnumMapSet (EnumMapSet, S(..))
import qualified Data.EnumMapSet as EMS

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :& b) where
    arbitrary = liftM2 (:&) arbitrary arbitrary
    shrink (x :& y) =    [ x' :& y | x' <- shrink x ]
                      ++ [ x :& y' | y' <- shrink y ]

instance (Arbitrary s) => Arbitrary (S s) where
    arbitrary = liftM S arbitrary

instance (Arbitrary k, EMS.Result k k () ~ (), EMS.IsKey k, EMS.SubKey k k ()) =>
    Arbitrary (EnumMapSet k) where
        arbitrary = fmap EMS.fromList $ listOf arbitrary

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

type TestEmm2 = EnumMapMap (ID2 :& K ID1) Int
type TestEms2 = EnumMapSet (ID2 :& S ID1)

main :: IO ()
main =
  hspec $ do
    -- Tests repeated from UnitEnumMap*. Here we're checking
    -- for instance overlaps
    describe "EnumMapMap SafeCopy instance" $ do
      let testEq :: TestEmm2 -> Bool
          testEq emm = op == Right emm
              where
                op = runGet safeGet $ runPut $ safePut emm
      prop "Leaves data intact" testEq
    describe "EnumMapSet SafeCopy Instance" $ do
      let testEq :: TestEms2 -> Bool
          testEq ems = op == Right ems
              where
                op = runGet safeGet $ runPut $ safePut ems
      prop "Leaves data intact" testEq
