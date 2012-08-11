{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.Internal
-- Copyright:   (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- Internal bits.  This interface is less stable and can change at any time.
-- In particular this means that while the rest of the postgresql-simple
-- package endeavors to follow the package versioning policy,  this module
-- does not.  Also, at the moment there are things in here that aren't
-- particularly internal and are exported elsewhere;  these will eventually
-- disappear from this module.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.Internal where

import Debug.Trace

import Prelude hiding (catch)

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.ByteString(ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (ord)
import           Data.Int (Int64)
import qualified Data.IntMap as IntMap
import           Data.String
import           Data.Typeable
import           Data.Word
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import qualified Data.Vector as V
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import           Database.SQLite.Simple.Types
import           Database.SQLite.Simple.Ok
import qualified Database.SQLite3 as Base



data Connection = Connection Base.Database


-- | A Field represents metadata about a particular field
--
-- You don't particularly want to retain these structures for a long
-- period of time,  as they will retain the entire query result,  not
-- just the field metadata

data Field = Field {
     result   :: Base.SQLData
   , column   :: {-# UNPACK #-} !Int
   }

data Row = Row {
     row        :: {-# UNPACK #-} !Int
   , rowresult  :: Result
   }

newtype RowParser a = RP { unRP :: ReaderT Row (StateT Int Ok) a }
   deriving ( Functor, Applicative, Alternative, Monad )

-- TODO would be better to have some other way of storing results.
-- This requires storage for all columns x rows.
--
-- Would be nice to get rid of getvalue, nfields and ntuples
-- altogether.  This is legacy from postgresql-simple where these
-- functions actually operate on actual PostgreSQL Result objects.
type Result = [[Base.SQLData]]

getvalue :: Result -> Int -> Int -> Maybe ByteString
getvalue r r_ c_ = sqldataToByteString $ (r !! r_) !! c_

nfields :: Result -> Int
nfields r = length . head $ r

gettypename :: Base.SQLData -> ByteString
gettypename (Base.SQLInteger _) = "INTEGER"
gettypename (Base.SQLFloat _) = "FLOAT"
gettypename (Base.SQLText _) = "TEXT"
gettypename (Base.SQLBlob _) = "BLOB"
gettypename Base.SQLNull = "NULL"

-- TODO this is horrible a kludge!!  There should be no need for any
-- conversion here.  Should just take an int and use that value
-- directly.
sqldataToByteString :: Base.SQLData -> Maybe ByteString
sqldataToByteString (Base.SQLInteger v) = Just $ (B8.pack (show v))
sqldataToByteString (Base.SQLText s) = Just . B8.pack $ s
sqldataToByteString Base.SQLNull = Nothing

utf8ToString = T.unpack . TE.decodeUtf8

exec :: Connection -> ByteString -> IO Result
exec (Connection conn) q = do
  rows <- bracket (Base.prepare conn (utf8ToString q)) Base.finalize takeRows
  return $ rows
    where
      takeRows stmt = do
        res <- Base.step stmt
        case res of
          Base.Row -> do
            cols <- Base.columns stmt
            next <- takeRows stmt
            return $ cols : next
          Base.Done ->
            return []
