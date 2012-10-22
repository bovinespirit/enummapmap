{-# LANGUAGE CPP, BangPatterns, FlexibleInstances, GeneralizedNewtypeDeriving,
  MagicHash, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapMap.Lazy
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Lazy 'EnumMapMap'.  Based upon "Data.IntMap.Lazy", this version uses multi
-- dimensional keys and 'Enum' types instead of 'Int's.  Keys are built using
-- the ':&' operator and terminated with 'K'.  They are stored using 'Int's so 2
-- keys that 'Enum' to the same 'Int' value will overwrite each other.  The
-- intension is that the 'Enum' types will actually be @newtype 'Int'@s.
--
-- > newtype AppleID = AppleID Int
-- > newtype TreeID = TreeID Int
-- > type Orchard = EnumMapMap (TreeID :& K AppleID) Apple
-- > apple = lookup (TreeID 4 :& K AppleID 32) orchard
--
-- The 'K' type is different to that used in "Data.EnumMapMap.Strict" so only lazy
-- operations can be performed on a lazy 'EnumMapMap'.
--
-- The functions are lazy on values, but strict on keys.
-----------------------------------------------------------------------------

module Data.EnumMapMap.Lazy (

            emptySubTrees,

            -- * Key types
            (:&)(..), K(..),
            d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,
            -- * Map Type
            EnumMapMap,
            -- * Query
            size,
            null,
            member,
            lookup,
            -- * Construction
            empty,
            singleton,
            -- * Insertion
            insert,
            insertWith,
            insertWithKey,
            -- * Delete\/Update
            delete,
            alter,
            -- * Combine
            -- ** Union
            union,
            unionWith,
            unionWithKey,
            unions,
            -- ** Difference
            difference,
            differenceWith,
            differenceWithKey,
            -- ** Intersection
            intersection,
            intersectionWith,
            intersectionWithKey,
            -- * Map
            map,
            mapWithKey,
            -- * Folds
            foldr,
            foldrWithKey,
            -- * Lists
            toList,
            fromList,
            keys,
            elems,
            keysSet,
            -- * Split/Join Keys
            toK,
            toS,
            splitKey,
            joinKey,
            unsafeJoinKey
) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null,init)

import           Control.DeepSeq (NFData(rnf))
import           Data.Bits

import           Data.EnumMapMap.Base
import qualified Data.EnumMapSet.Base as EMS

-- | Keys are terminated with the 'K' type
--
-- > singleKey :: K Int
-- > singleKey = K 5
--
newtype K k = K k
           deriving (Show, Eq)

instance (Enum k, Eq k) => IsKey (K k) where
    data EnumMapMap (K k) v = KEC (EMM k v)

    emptySubTrees e@(KEC emm) =
        case emm of
          Nil -> False
          _   -> emptySubTrees_ e
    emptySubTrees_ (KEC emm) = go emm
        where
          go t = case t of
                   Bin _ _ l r -> go l || go r
                   Tip _ _     -> False
                   Nil         -> True

    removeEmpties = id

    unsafeJoinKey (KEC emm) = KCC emm

    empty = KEC Nil

    null (KEC t) = case t of
                     Nil -> True
                     _   -> False

    size (KEC t) = go t
        where
          go (Bin _ _ l r) = go l + go r
          go (Tip _ _)     = 1
          go Nil           = 0

    member !(K key') (KEC emm) = go emm
        where
          go t = case t of
                   Bin _ m l r -> case zero key m of
                                    True  -> go l
                                    False -> go r
                   Tip kx _    -> key == kx
                   Nil         -> False
          key = fromEnum key'

    singleton !(K key) = KEC . Tip (fromEnum key)

    insert !(K key') val (KEC emm) = KEC $ go emm
        where
          go t = case t of
                   Bin p m l r
                       | nomatch key p m -> join key (Tip key val) p t
                       | zero key m      -> Bin p m (go l) r
                       | otherwise       -> Bin p m l (go r)
                   Tip ky _
                       | key == ky       -> Tip key val
                       | otherwise       -> join key (Tip key val) ky t
                   Nil                   -> Tip key val
          key = fromEnum key'

    insertWithKey f k@(K key') val (KEC emm) = KEC $ go emm
        where go t = case t of
                     Bin p m l r
                         | nomatch key p m -> join key (Tip key val) p t
                         | zero key m      -> Bin p m (go l) r
                         | otherwise       -> Bin p m l (go r)
                     Tip ky y
                         | key == ky       -> Tip key (f k val y)
                         | otherwise       -> join key (Tip key val) ky t
                     Nil                   -> Tip key val
              key = fromEnum key'

    alter f !(K key') (KEC emm) = KEC $ go emm
        where
          go t = case t of
                Bin p m l r
                    |nomatch key p m -> case f Nothing of
                                          Nothing -> t
                                          Just x  -> join key (Tip key x) p t
                    | zero key m     -> bin p m (go l) r
                    | otherwise      -> bin p m l (go r)
                Tip ky y
                    | key == ky      -> case f (Just y) of
                                          Just x  -> Tip ky x
                                          Nothing -> Nil
                    | otherwise      -> case f Nothing of
                                          Just x  -> join key (Tip key x) ky t
                                          Nothing -> Tip ky y
                Nil                  -> case f Nothing of
                                          Just x  -> Tip key x
                                          Nothing -> Nil
            where
              key = fromEnum key'

    mapWithKey f (KEC emm) = KEC $ mapWithKey_ (\k -> f $ K k) emm
    foldr f init (KEC emm) =
        case emm of Bin _ m l r | m < 0 -> go (go init l) r -- put negative numbers before
                                | otherwise -> go (go init r) l
                    _          -> go init emm
        where
          go z' Nil           = z'
          go z' (Tip _ x)     = f x z'
          go z' (Bin _ _ l r) = go (go z' r) l
    foldrWithKey f init (KEC emm) = foldrWithKey_ (\k -> f $ K k) init emm
    keysSet (KEC emm) = EMS.KSC $ go emm
        where
          go Nil        = EMS.Nil
          go (Tip kx _) = EMS.Tip (EMS.prefixOf kx) (EMS.bitmapOf kx)
          go (Bin p m l r)
              | m .&. EMS.suffixBitMask == 0 = EMS.Bin p m (go l) (go r)
              | otherwise = EMS.Tip (p .&. EMS.prefixBitMask)
                            (computeBm (computeBm 0 l) r)
              where
                computeBm !acc (Bin _ _ l' r') = computeBm (computeBm acc l') r'
                computeBm !acc (Tip kx _)      = acc .|. EMS.bitmapOf kx
                computeBm !acc Nil             = acc

    union (KEC emm1) (KEC emm2) = KEC $ mergeWithKey' Bin const id id emm1 emm2
    unionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' Bin go id id emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ f (K $ toEnum k1) x1 x2

    difference (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin (\_ _ -> Nil) id (const Nil) emm1 emm2
    differenceWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin combine id (const Nil) emm1 emm2
            where
              combine = \(Tip k1 x1) (Tip _ x2)
                      -> case f (K $ toEnum k1) x1 x2 of
                           Nothing -> Nil
                           Just x -> Tip k1 x

    intersection (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin const (const Nil) (const Nil) emm1 emm2
    intersectionWithKey f (KEC emm1) (KEC emm2) =
        KEC $ mergeWithKey' bin go (const Nil) (const Nil) emm1 emm2
            where
              go = \(Tip k1 x1) (Tip _ x2) ->
                   Tip k1 $ f (K $ toEnum k1) x1 x2

    equal (KEC emm1) (KEC emm2) = emm1 == emm2
    nequal (KEC emm1) (KEC emm2) = emm1 /= emm2

{---------------------------------------------------------------------
 Instances
---------------------------------------------------------------------}

instance (Show v) => Show (EnumMapMap (K k) v) where
    show (KEC emm) = show emm

instance NFData v => NFData (EnumMapMap (K k) v) where
    rnf (KEC emm) = go emm
        where
          go Nil           = ()
          go (Tip _ v)     = rnf v
          go (Bin _ _ l r) = go l `seq` go r

instance HasSKey (K k) where
    type Skey (K k) = EMS.S k
    toS (K !k) = EMS.S k
    toK (EMS.S !k) = K k

{---------------------------------------------------------------------
 Split/Join Keys
---------------------------------------------------------------------}

type instance Plus (K k1) k2 = k1 :& k2

instance IsSplit (k :& t) Z where
    type Head (k :& t) Z = K k
    type Tail (k :& t) Z = t
    splitKey Z (KCC emm) = KEC $ emm

instance (Enum k1, k1 ~ k2) => SubKey (K k1) (k2 :& t2) v where
    type Result (K k1) (k2 :& t2) v = EnumMapMap t2 v
    lookup (K key') (KCC emm) = lookup_ (fromEnum key') emm
    delete !(K key') (KCC emm) = KCC $ delete_ (fromEnum key') emm

instance (Enum k) => SubKey (K k) (K k) v where
    type Result (K k) (K k) v = v
    lookup (K key') (KEC emm) = lookup_ (fromEnum key') emm
    delete !(K key') (KEC emm) = KEC $ delete_ (fromEnum key') emm

lookup_ :: Key -> EMM k v -> Maybe v
lookup_ !key emm =
    case emm of
      Bin _ m l r
          | zero key m -> lookup_ key l
          | otherwise  -> lookup_ key r
      Tip kx x         -> if kx == key then Just x else Nothing
      Nil              -> Nothing

delete_ :: Key -> EMM k v -> EMM k v
delete_ !key emm =
    case emm of
      Bin p m l r | nomatch key p m -> emm
                  | zero key m      -> bin p m (delete_ key l) r
                  | otherwise       -> bin p m l (delete_ key r)
      Tip ky _    | key == ky       -> Nil
                  | otherwise       -> emm
      Nil                           -> Nil
