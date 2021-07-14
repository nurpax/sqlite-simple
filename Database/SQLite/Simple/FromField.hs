
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables      #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
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
    , fieldData
    , returnError
    ) where

import           Control.Exception (SomeException(..), Exception)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Time (UTCTime, Day)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Float (double2Float)

import           Database.SQLite3 as Base
import           Database.SQLite.Simple.Types
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.Time

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

type FieldParser a = Field -> Ok a

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
    fromField (Field SQLNull _) = pure Nothing
    fromField f                 = Just <$> fromField f

instance FromField Null where
    fromField (Field SQLNull _) = pure Null
    fromField f                 = returnError ConversionFailed f "data is not null"

takeInt :: (Num a, Typeable a) => Field -> Ok a
takeInt (Field (SQLInteger i) _) = Ok . fromIntegral $ i
takeInt f                        = returnError ConversionFailed f "need an int"

instance FromField Int8 where
    fromField = takeInt

instance FromField Int16 where
    fromField = takeInt

instance FromField Int32 where
    fromField = takeInt

instance FromField Int where
    fromField = takeInt

instance FromField Int64 where
    fromField = takeInt

instance FromField Integer where
    fromField = takeInt

instance FromField Word8 where
    fromField = takeInt

instance FromField Word16 where
    fromField = takeInt

instance FromField Word32 where
    fromField = takeInt

instance FromField Word64 where
    fromField = takeInt

instance FromField Word where
    fromField = takeInt

instance FromField Double where
    fromField (Field (SQLFloat flt) _) = Ok flt
    fromField f                        = returnError ConversionFailed f "expecting an SQLFloat column type"

instance FromField Float where
    fromField (Field (SQLFloat flt) _) = Ok . double2Float $ flt
    fromField f                        = returnError ConversionFailed f "expecting an SQLFloat column type"

instance FromField Bool where
    fromField f@(Field (SQLInteger b) _)
      | (b == 0) || (b == 1) = Ok (b /= 0)
      | otherwise = returnError ConversionFailed f ("bool must be 0 or 1, got " ++ show b)

    fromField f = returnError ConversionFailed f "expecting an SQLInteger column type"

instance FromField T.Text where
  fromField f@(Field sqlData _) = case sqlData of
    SQLText v -> Ok v
    SQLInteger v -> Ok $ T.pack $ show v
    SQLFloat v -> Ok $ T.pack $ show v
    _ -> returnError ConversionFailed f "need convertible to text"

instance FromField LT.Text where
  fromField f = LT.fromStrict <$> fromField f

instance FromField [Char] where
  fromField f = T.unpack <$> fromField f

instance FromField ByteString where
  fromField (Field (SQLBlob blb) _) = Ok blb
  fromField f                       = returnError ConversionFailed f "expecting SQLBlob column type"

instance FromField LB.ByteString where
  fromField (Field (SQLBlob blb) _) = Ok . LB.fromChunks $ [blb]
  fromField f                       = returnError ConversionFailed f "expecting SQLBlob column type"

instance FromField UTCTime where
  fromField f@(Field (SQLText t) _) =
    case parseUTCTime t of
      Right t -> Ok t
      Left e -> returnError ConversionFailed f ("couldn't parse UTCTime field: " ++ e)

  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance FromField Day where
  fromField f@(Field (SQLText t) _) =
    case parseDay t of
      Right t -> Ok t
      Left e -> returnError ConversionFailed f ("couldn't parse Day field: " ++ e)

  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance FromField SQLData where
  fromField f = Ok (fieldData f)

fieldTypename :: Field -> String
fieldTypename = B.unpack . gettypename . result

-- | Return the actual SQL data for a database field.  This allows
-- user-defined 'FromField' instances to access the SQL data
-- associated with a field being parsed.
fieldData :: Field -> SQLData
fieldData = result

-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Ok a
returnError mkErr f = left . mkErr (fieldTypename f)
                                   (show (typeOf (undefined :: a)))
