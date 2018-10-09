{-# LANGUAGE RecordWildCards, DefaultSignatures, FlexibleContexts,
  StandaloneDeriving #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.FromRow
-- Copyright:   (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'FromRow' typeclass, for converting a row of results
-- returned by a SQL query into a more useful Haskell representation.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.
------------------------------------------------------------------------------

module Database.SQLite.Simple.FromRow
     ( GFromRow(..)
     , FromRow(..)
     , RowParser
     , field
     , fieldWith
     , numFieldsRemaining
     ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Exception (SomeException(..))
import           Control.Monad (replicateM)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           GHC.Generics

import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.Types


-- | Generic derivation of 'FromRow'.
--
-- Instantiating 'FromRow' can in some cases be quite tedious. Luckily
-- we can derive it generically in some cases where the type at hand
-- has a 'Generic' instance.  The current implementation only works
-- for a (n-ary) product types.  So we would not be able to
-- e.g. derive a 'FromRow' instance for
--
-- @
-- data Bool = True | False
-- @
--
-- We /can/, however, derive a generic instance for the @User@ type
-- (see the example in 'FromRow').
--
-- @since 0.4.16.1
class GFromRow f where
  gfromRow :: RowParser (f a)

instance GFromRow U1 where
  gfromRow = pure U1

instance FromField a => GFromRow (K1 i a) where
  gfromRow = K1 <$> field

instance GFromRow a => GFromRow (M1 i c a) where
  gfromRow = M1 <$> gfromRow

instance (GFromRow a, GFromRow b) => GFromRow (a :*: b) where
  gfromRow = (:*:) <$> gfromRow <*> gfromRow

-- | A collection type that can be converted from a sequence of fields.
-- Instances are provided for tuples up to 10 elements and lists of any length.
--
-- Note that instances can defined outside of sqlite-simple,  which is
-- often useful.   For example, here's an instance for a user-defined pair:
--
-- @
-- data User = User { name :: String, fileQuota :: Int }
--
-- instance 'FromRow' User where
--     fromRow = User \<$\> 'field' \<*\> 'field'
-- @
--
-- The number of calls to 'field' must match the number of fields returned
-- in a single row of the query result.  Otherwise,  a 'ConversionFailed'
-- exception will be thrown.
--
-- Note the caveats associated with user-defined implementations of
-- 'fromRow'.
--
-- === Generic implementation
--
-- Since version 0.4.16.1 it is possible in some cases to derive a
-- generic implementation for 'FromRow'.  With a 'Generic' instance
-- for @User@, the example above could be written:
--
-- @
-- instance 'FromRow' User where
-- @
--
-- With @-XDeriveAnyClass -XDerivingStrategies@ the same can be written:
--
-- @
-- deriving anyclass instance 'FromRow' User
-- @
--
-- For more details refer to 'GFromRow'.
class FromRow a where
    fromRow :: RowParser a

    default fromRow :: Generic a => GFromRow (Rep a) => RowParser a
    fromRow = to <$> gfromRow

fieldWith :: FieldParser a -> RowParser a
fieldWith fieldP = RP $ do
    ncols <- asks nColumns
    (column, remaining) <- lift get
    lift (put (column + 1, tail remaining))
    if column >= ncols
    then
      lift (lift (Errors [SomeException (ColumnOutOfBounds (column+1))]))
    else do
      let r = head remaining
          field = Field r column
      lift (lift (fieldP field))

field :: FromField a => RowParser a
field = fieldWith fromField

numFieldsRemaining :: RowParser Int
numFieldsRemaining = RP $ do
  ncols <- asks nColumns
  (columnIdx,_) <- lift get
  return $! ncols - columnIdx

instance (FromField a) => FromRow (Only a) where
    fromRow = Only <$> field

instance (FromField a, FromField b) => FromRow (a,b) where
    fromRow = (,) <$> field <*> field

instance (FromField a, FromField b, FromField c) => FromRow (a,b,c) where
    fromRow = (,,) <$> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d) =>
    FromRow (a,b,c,d) where
    fromRow = (,,,) <$> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e) =>
    FromRow (a,b,c,d,e) where
    fromRow = (,,,,) <$> field <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f) =>
    FromRow (a,b,c,d,e,f) where
    fromRow = (,,,,,) <$> field <*> field <*> field <*> field <*> field
                      <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g) =>
    FromRow (a,b,c,d,e,f,g) where
    fromRow = (,,,,,,) <$> field <*> field <*> field <*> field <*> field
                       <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h) =>
    FromRow (a,b,c,d,e,f,g,h) where
    fromRow = (,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                        <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i) =>
    FromRow (a,b,c,d,e,f,g,h,i) where
    fromRow = (,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                         <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j) =>
    FromRow (a,b,c,d,e,f,g,h,i,j) where
    fromRow = (,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field

instance FromField a => FromRow [a] where
    fromRow = do
      n <- numFieldsRemaining
      replicateM n field

instance (FromRow a, FromRow b) => FromRow (a :. b) where
    fromRow = (:.) <$> fromRow <*> fromRow
