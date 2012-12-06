{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.Internal
-- Copyright:   (c) 2011-2012 Leon P Smith
--              (c) 2012 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Internal bits.  This interface is less stable and can change at any time.
-- In particular this means that while the rest of the sqlite-simple
-- package endeavors to follow the package versioning policy,  this module
-- does not.  Also, at the moment there are things in here that aren't
-- particularly internal and are exported elsewhere;  these will eventually
-- disappear from this module.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.Internal where

import           Prelude hiding (catch)

import           Control.Applicative
import           Control.Monad.Fix (fix)
import           Control.Exception
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8()
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader

import qualified Data.Text          as T

import           Database.SQLite.Simple.Ok
import qualified Database.SQLite3 as Base

-- | Connection to an open database.
data Connection = Connection Base.Database

-- | A Field represents metadata about a particular field

data Field = Field {
     result   :: Base.SQLData
   , column   :: {-# UNPACK #-} !Int
   }

data Row = Row {
     row        :: {-# UNPACK #-} !Int
   , rowresult  :: [Base.SQLData]
   }

newtype RowParser a = RP { unRP :: ReaderT Row (StateT Int Ok) a }
   deriving ( Functor, Applicative, Alternative, Monad )

type Result = [[Base.SQLData]]

gettypename :: Base.SQLData -> ByteString
gettypename (Base.SQLInteger _) = "INTEGER"
gettypename (Base.SQLFloat _) = "FLOAT"
gettypename (Base.SQLText _) = "TEXT"
gettypename (Base.SQLBlob _) = "BLOB"
gettypename Base.SQLNull = "NULL"

exec :: Connection -> T.Text -> IO Result
exec (Connection conn) q =
  bracket (Base.prepare conn q) Base.finalize stepStmt


-- Run a query on a prepared statement
stepStmt :: Base.Statement -> IO Result
stepStmt stmt =
  flip fix [] $ \loop acc -> do
    res <- Base.step stmt
    case res of
      Base.Done ->
        return (reverse acc)
      Base.Row -> do
        !cols <- Base.columns stmt
        loop (cols : acc)
