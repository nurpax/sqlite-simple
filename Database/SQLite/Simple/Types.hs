{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, CPP #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.Types
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- Top-level module for sqlite-simple.
--
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.Types
    (
      Null(..)
    , Only(..)
    , Query(..)
    , (:.)(..)
    ) where

import           Control.Arrow (first)
import           Data.String (IsString(..))
import           Data.Tuple.Only (Only(..))
import           Data.Typeable (Typeable)
import qualified Data.Text as T

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

-- | A placeholder for the SQL @NULL@ value.
data Null = Null
          deriving (Read, Show, Typeable)

instance Eq Null where
    _ == _ = False
    _ /= _ = False

-- | A query string. This type is intended to make it difficult to
-- construct a SQL query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce SQL injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.SQLite.Simple
-- >
-- > q :: Query
-- > q = "select ?"
--
-- The underlying type is a 'Text', and literal Haskell strings that
-- contain Unicode characters will be correctly transformed to UTF-8.
newtype Query = Query {
      fromQuery :: T.Text
    } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . T.pack

instance Semigroup Query where
    Query a <> Query b = Query (T.append a b)
    {-# INLINE (<>) #-}

instance Monoid Query where
    mempty = Query T.empty
    mappend = (<>)
    {-# INLINE mappend #-}

-- | A composite type to parse your custom data structures without
-- having to define dummy newtype wrappers every time.
--
--
-- > instance FromRow MyData where ...
--
-- > instance FromRow MyData2 where ...
--
--
-- then I can do the following for free:
--
-- @
-- res <- query' c "..."
-- forM res $ \\(MyData{..} :. MyData2{..}) -> do
--   ....
-- @
data h :. t = h :. t deriving (Eq,Ord,Show,Read,Typeable)

infixr 3 :.
