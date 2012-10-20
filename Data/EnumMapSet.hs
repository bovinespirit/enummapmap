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
-- Based on "Data.IntSet", this module provides multi-dimensional sets of
-- 'Enums'. Keys are built using ':&' and terminated with 'S'.  They are stored
-- using 'Int's so 2 keys that 'Enum' to the same 'Int' value will overwrite
-- each other.  The intension is that the 'Enum' types will actually be @newtype
-- 'Int'@s.
--
--
-- > newtype AppleID = AppleID Int
-- > newtype TreeID = TreeID Int
-- > type Orchard = EnumMapSet (TreeID :& S AppleID)
-- > applePresent = member (TreeID 4 :& K AppleID 32) orchard
--
-----------------------------------------------------------------------------

module Data.EnumMapSet (
            EnumMapSet,
            S(..), (:&)(..),
            -- * Query
            EMS.null,
            size,
            member,
            EMS.lookup,
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
