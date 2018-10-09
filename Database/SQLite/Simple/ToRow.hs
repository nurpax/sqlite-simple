{-# Language DefaultSignatures, FlexibleContexts, DerivingStrategies,
  DeriveAnyClass, StandaloneDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.ToRow
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'ToRow' typeclass, for rendering a collection of
-- parameters to a SQL query.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.ToRow
    ( GToRow(..)
    , ToRow(..)
    ) where

import GHC.Generics

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.Types (Only(..), (:.)(..))

import Database.SQLite3 (SQLData(..))

-- | Generic derivation of 'ToRow'.  For details about what can be
-- derived refer to 'Database.Sqlite.Simple.FromRow.GFromRow'.
--
-- @since 0.4.16.1
class GToRow f where
  gtoRow :: (f a) -> [SQLData]

instance GToRow U1 where
  gtoRow U1 = toRow ()

instance ToField a => GToRow (K1 i a) where
  gtoRow (K1 a) = pure $ toField a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gtoRow (a :*: b) = gtoRow a <> gtoRow b

instance GToRow a => GToRow (M1 i c a) where
  gtoRow (M1 a) = gtoRow a

-- | A collection type that can be turned into a list of 'SQLData'
-- elements.
--
-- Since version 0.4.16.1 it is possible in some cases to derive a
-- generic implementation for 'ToRow'.  Refer to the documentation for
-- 'Database.Sqlite.Simple.FromRow.FromRow' to see how this can be
-- done.
class ToRow a where
    toRow :: a -> [SQLData]
    -- ^ 'ToField' a collection of values.

    default toRow :: Generic a => GToRow (Rep a) => a -> [SQLData]
    toRow a = gtoRow $ from a

deriving anyclass instance ToRow ()
deriving anyclass instance (ToField a) => ToRow (Only a)
deriving anyclass instance (ToField a, ToField b) => ToRow (a,b)
deriving anyclass instance (ToField a, ToField b, ToField c) => ToRow (a,b,c)
deriving anyclass instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a,b,c,d)
deriving anyclass instance (ToField a, ToField b, ToField c, ToField d, ToField e) => ToRow (a,b,c,d,e)
deriving anyclass instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f) => ToRow (a,b,c,d,e,f)
deriving anyclass instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g) => ToRow (a,b,c,d,e,f,g)

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h)
    => ToRow (a,b,c,d,e,f,g,h) where
    toRow (a,b,c,d,e,f,g,h) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i)
    => ToRow (a,b,c,d,e,f,g,h,i) where
    toRow (a,b,c,d,e,f,g,h,i) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j)
    => ToRow (a,b,c,d,e,f,g,h,i,j) where
    toRow (a,b,c,d,e,f,g,h,i,j) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j]

instance (ToField a) => ToRow [a] where
    toRow = map toField

instance (ToRow a, ToRow b) => ToRow (a :. b) where
    toRow (a :. b) = toRow a ++ toRow b
