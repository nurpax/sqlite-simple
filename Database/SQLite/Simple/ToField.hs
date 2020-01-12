{-# LANGUAGE DeriveDataTypeable, DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'ToField' typeclass, for rendering a parameter to an SQLite
-- value to be bound as a SQL query parameter.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.ToField (ToField(..)) where

import           Blaze.ByteString.Builder (toByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import           Data.Time (Day, UTCTime)
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Float

import           Database.SQLite3 as Base
import           Database.SQLite.Simple.Types (Null)
import           Database.SQLite.Simple.Time

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
    toField :: a -> SQLData
    -- ^ Prepare a value for substitution into a query string.

instance ToField SQLData where
    toField a = a
    {-# INLINE toField #-}

instance (ToField a) => ToField (Maybe a) where
    toField Nothing  = Base.SQLNull
    toField (Just a) = toField a
    {-# INLINE toField #-}

instance ToField Null where
    toField _ = Base.SQLNull
    {-# INLINE toField #-}

instance ToField Bool where
    toField False = SQLInteger 0
    toField True  = SQLInteger 1
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

instance ToField SB.ByteString where
    toField = SQLBlob
    {-# INLINE toField #-}

instance ToField LB.ByteString where
    toField = toField . SB.concat . LB.toChunks
    {-# INLINE toField #-}

instance ToField T.Text where
    toField = SQLText
    {-# INLINE toField #-}

instance ToField [Char] where
    toField = SQLText . T.pack
    {-# INLINE toField #-}

instance ToField LT.Text where
    toField = toField . LT.toStrict
    {-# INLINE toField #-}

instance ToField UTCTime where
    toField = SQLText . T.decodeUtf8 . toByteString . utcTimeToBuilder
    {-# INLINE toField #-}

instance ToField Day where
    toField = SQLText . T.decodeUtf8 . toByteString . dayToBuilder
    {-# INLINE toField #-}

-- TODO enable these
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
