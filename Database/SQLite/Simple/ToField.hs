{-# LANGUAGE DeriveDataTypeable, DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'ToField' typeclass, for rendering a parameter to a SQL query.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.ToField
    (
      Action
    , ToField(..)
    ) where

import           Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import           Blaze.ByteString.Builder.Char8 (fromChar)
import           Blaze.Text (integral, double, float)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List (intersperse)
import           Data.Monoid (mappend)
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import           Data.Time (Day, TimeOfDay, LocalTime, UTCTime, ZonedTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           GHC.Float

import           Database.SQLite3 as Base
import           Database.SQLite.Simple.Time
import           Database.SQLite.Simple.Types (Binary(..), In(..), Null)

-- | How to bind elements for query.
type Action = SQLData

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
    toField :: a -> Action
    -- ^ Prepare a value for substitution into a query string.

instance ToField Action where
    toField a = a
    {-# INLINE toField #-}

instance (ToField a) => ToField (Maybe a) where
    toField Nothing  = renderNull
    toField (Just a) = toField a
    {-# INLINE toField #-}

instance (ToField a) => ToField (In [a]) where
    toField (In _) =
      error "NOT IMPLEMENTED see https://github.com/nurpax/sqlite-simple/issues/6"

renderNull :: Action
renderNull = Base.SQLNull

instance ToField Null where
    toField _ = renderNull
    {-# INLINE toField #-}

instance ToField Bool where
    toField True  = SQLText "true"
    toField False = SQLText "false"
    {-# INLINE toField #-}

instance ToField Int8 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Int16 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Int32 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Int where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Int64 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Integer where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Word8 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Word16 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Word32 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Word where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Word64 where
    toField = SQLInteger . fromIntegral
    {-# INLINE toField #-}

instance ToField Float where
    toField = SQLFloat . float2Double
    {-# INLINE toField #-}

instance ToField Double where
    toField = SQLFloat
    {-# INLINE toField #-}

instance ToField (Binary SB.ByteString) where
    toField (Binary bs) = SQLBlob bs
    {-# INLINE toField #-}

instance ToField (Binary LB.ByteString) where
    toField (Binary bs) = (SQLBlob . SB.concat . LB.toChunks) bs
    {-# INLINE toField #-}

instance ToField SB.ByteString where
    toField = SQLBlob
    {-# INLINE toField #-}

instance ToField LB.ByteString where
    toField = toField . SB.concat . LB.toChunks
    {-# INLINE toField #-}

-- TODO is it ok to stick text into blob fields in the db?
sqltextFromBS :: ByteString -> SQLData
sqltextFromBS = SQLBlob

instance ToField ST.Text where
    toField = sqltextFromBS . ST.encodeUtf8
    {-# INLINE toField #-}

instance ToField [Char] where
    toField = SQLText
    {-# INLINE toField #-}

instance ToField LT.Text where
    toField = toField . LT.toStrict
    {-# INLINE toField #-}

-- TODO enable these

--instance ToField UTCTime where
--    toField = SQLText . utcTimeToBuilder
--    {-# INLINE toField #-}
--
--instance ToField ZonedTime where
--    toField = SQLText . zonedTimeToBuilder
--    {-# INLINE toField #-}
--
--instance ToField LocalTime where
--    toField = SQLText . localTimeToBuilder
--    {-# INLINE toField #-}
--
--instance ToField Day where
--    toField = SQLText . dayToBuilder
--    {-# INLINE toField #-}
--
--instance ToField TimeOfDay where
--    toField = SQLText . timeOfDayToBuilder
--    {-# INLINE toField #-}
--
--instance ToField UTCTimestamp where
--    toField = SQLText . utcTimestampToBuilder
--    {-# INLINE toField #-}
--
--instance ToField ZonedTimestamp where
--    toField = SQLText . zonedTimestampToBuilder
--    {-# INLINE toField #-}
--
--instance ToField LocalTimestamp where
--    toField = SQLText . localTimestampToBuilder
--    {-# INLINE toField #-}
--
--instance ToField Date where
--    toField = SQLText . dateToBuilder
--    {-# INLINE toField #-}
