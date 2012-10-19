{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMapSet
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
--                (c) Matthew West 2012
-- License     :  BSD-style
-- Stability   :  experimental
-- Portability :  Uses GHC extensions
--
-- Based on Data.IntSet.hs
--
-----------------------------------------------------------------------------

module Data.EnumMapSet (
            EnumMapSet,
            S(..), (:&)(..),
            -- * Query
            EMS.null,
            size,
            member,
            -- * Construction
            empty,
            singleton,
            insert,
            delete,
            -- * Combine
            union,
            difference,
            intersection,
            -- * Map
            EMS.map,
            -- * Folds
            EMS.foldr,
            -- * Lists
            toList,
            fromList,
            keys
) where

import Data.EnumMapSet.Base as EMS
