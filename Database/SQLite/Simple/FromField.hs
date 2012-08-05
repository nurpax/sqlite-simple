{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables      #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'FromField' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- SQLite numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the SQLite's 32-bit
-- @Int@ type because it can represent a @Int@ exactly. On the other hand,
-- since a 'Double' might lose precision if representing a 64-bit @BigInt@,
-- the two are /not/ considered compatible.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.FromField
    (
      FromField(..)
    , FieldParser
    , ResultError(..)

    , Field
    ) where

#include "MachDeps.h"

import           Control.Applicative
                   ( Applicative, (<|>), (<$>), pure )
import           Control.Exception (SomeException(..), Exception)
import           Data.Attoparsec.Char8 hiding (Result)
import           Data.Bits ((.&.), (.|.), shiftL)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Int (Int16, Int32, Int64)
import           Data.List (foldl')
import           Data.Ratio (Ratio)
import           Data.Time ( UTCTime, ZonedTime, LocalTime, Day, TimeOfDay )
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64)
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok --
--import           Database.SQLite.Simple.Types (Binary(..), Null(..))
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: String
                                , errHaskellType :: String
                                , errMessage :: String }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: String
                                  , errHaskellType :: String
                                  , errMessage :: String }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: String
                                    , errHaskellType :: String
                                    , errMessage :: String }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

left :: Exception a => a -> Ok b
left = Errors . (:[]) . SomeException

type FieldParser a = Field -> Maybe ByteString -> Ok a

-- | A type that may be converted from a SQL type.
class FromField a where
    fromField :: FieldParser a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Returns a list of exceptions if the conversion fails.  In the case of
    -- library instances,  this will usually be a single 'ResultError',  but
    -- may be a 'UnicodeException'.
    --
    -- Implementations of 'fromField' should not retain any references to
    -- the 'Field' nor the 'ByteString' arguments after the result has
    -- been evaluated to WHNF.  Such a reference causes the entire
    -- @LibPQ.'PQ.Result'@ to be retained.
    --
    -- For example,  the instance for 'ByteString' uses 'B.copy' to avoid
    -- such a reference,  and that using bytestring functions such as 'B.drop'
    -- and 'B.takeWhile' alone will also trigger this memory leak.

instance (FromField a) => FromField (Maybe a) where
    fromField _ Nothing = pure Nothing
    fromField f bs      = Just <$> fromField f bs

--instance FromField Null where
--    fromField _ Nothing  = pure Null
--    fromField f (Just _) = returnError ConversionFailed f "data is not null"
