{-# LANGUAGE CPP, MagicHash, TypeFamilies, TypeOperators, BangPatterns,
             FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapMap.Lazy
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
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
            -- * Traversal
            -- ** Map
            map,
            mapWithKey,
            -- * Folds
            foldrWithKey,
            -- * Lists
            toList,
            fromList,
            -- * Split/Join Keys
            splitKey,
            joinKey,
            unsafeJoinKey
) where

import           Prelude hiding (lookup,map,filter,foldr,foldl,null, init)

import           Data.EnumMapMap.Base

instance (Enum k) => IsEmm (K k) where
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

    lookup !(K key') (KEC emm) = go emm
        where
          go (Bin _ m l r)
              | zero key m = go l
              | otherwise = go r
          go (Tip kx x)
             = case kx == key of
                 True -> Just x
                 False -> Nothing
          go Nil = Nothing
          key = fromEnum key'

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

    delete !(K key') (KEC emm) = KEC $ go emm
        where
          go t = case t of
                   Bin p m l r | nomatch key p m -> t
                               | zero key m      -> bin p m (go l) r
                               | otherwise       -> bin p m l (go r)
                   Tip ky _    | key == ky       -> Nil
                               | otherwise       -> t
                   Nil                           -> Nil
          key = fromEnum key'

    mapWithKey f (KEC emm) = KEC $ mapWithKey_ (\k -> f $ K k) emm
    foldrWithKey f init (KEC emm) = foldrWithKey_ (\k -> f $ K k) init emm

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

{---------------------------------------------------------------------
 Split/Join Keys
---------------------------------------------------------------------}

type instance Plus (K k1) k2 = k1 :& k2

instance IsSplit (k :& t) Z where
    type Head (k :& t) Z = K k
    type Tail (k :& t) Z = t
    splitKey Z (KCC emm) = KEC $ emm
