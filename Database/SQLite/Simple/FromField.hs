{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables      #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
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

-- TODO
import Debug.Trace

#include "MachDeps.h"

import           Control.Applicative
                   ( Applicative, (<|>), (<$>), pure )
import           Control.Exception (SomeException(..), Exception)
import           Data.Attoparsec.Char8 hiding (Result)
import           Data.Bits ((.&.), (.|.), shiftL)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import           Data.Int (Int16, Int32, Int64)
import           Data.List (foldl')
import           Data.Ratio (Ratio)
import           Data.Time ( UTCTime, ZonedTime, LocalTime, Day, TimeOfDay )
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64)
import           System.IO.Unsafe (unsafePerformIO)

import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

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

instance FromField Int where
    fromField = atto okInt $ signed decimal

instance FromField ST.Text where
    fromField f = doFromField f okText $ (either left pure . ST.decodeUtf8')
    -- FIXME:  check character encoding

instance FromField [Char] where
    fromField f dat = ST.unpack <$> fromField f dat


newtype Compat = Compat Word64

-- TODO should move these to direct-sqlite?
data BuiltinType =
  SQL3Int | SQL3Float | SQL3Text | SQL3Blob
  deriving (Enum)

mkCompats :: [BuiltinType] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: BuiltinType -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0

okText, okBinary, ok16, ok32, ok64, okInt :: Compat
okText   = mkCompats [SQL3Text]
okBinary = mkCompats [SQL3Blob]
ok16 = mkCompats [SQL3Int]
ok32 = mkCompats [SQL3Int]
ok64 = mkCompats [SQL3Int]
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif

doFromField :: forall a . (Typeable a)
          => Field -> Compat -> (ByteString -> Ok a)
          -> Maybe ByteString -> Ok a
doFromField f types cvt (Just bs) | otherwise = cvt bs
--    | mkCompat typ `compat` types = cvt bs
--    | otherwise = returnError Incompatible f "types incompatible"


doFromField f _ _ _ = returnError UnexpectedNull f ""


fieldTypename :: Field -> String
fieldTypename = B.unpack . gettypename . result

-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Ok a
returnError mkErr f = left . mkErr (fieldTypename f)
                                   (show (typeOf (undefined :: a)))

atto :: forall a. (Typeable a)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Ok a
atto types p0 f dat = doFromField f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Ok a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> pure v
