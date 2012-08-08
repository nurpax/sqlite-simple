{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.SQLite.Simple (
    open
  , close
  , query_
  , field
  , Query
  , Connection
  , FromRow
  , In(..)
  , Binary(..)
  , Only(..)
    -- ** Exceptions
  , FormatError(fmtMessage, fmtQuery, fmtParams)
  ) where

import Debug.Trace

import Control.Applicative
import Control.Exception
  ( Exception, onException, throw, throwIO, finally )
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base

import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow (FromRow(..))

--import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

open :: String -> IO Connection
open fname = Connection <$> Base.open fname

close :: Connection -> IO ()
close (Connection c) = Base.close c


-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn q@(Query que) = do
  result <- exec conn que
  finishQuery conn q result


forM' :: (Ord n, Num n) => n -> n -> (n -> IO a) -> IO [a]
forM' lo hi m = loop hi []
  where
    loop !n !as
      | n < lo = return as
      | otherwise = do
           a <- m n
           loop (n-1) (a:as)

finishQuery :: (FromRow r) => Connection -> Query -> Result -> IO [r]
finishQuery conn q result = do
  -- TODO handle sqlite errors, this just skips all of that
  let ncols = nfields result
      nrows = ntuples result
  typenames <- V.generateM ncols (\col -> return "NOT IMPLEMENTED")
  -- TODO missing typenames!!
--                           (\col -> do
--                              getTypename conn =<< PQ.ftype result col)
  forM' 0 (nrows-1) $ \row -> do
     let rw = Row row typenames result
     case runStateT (runReaderT (unRP fromRow) rw) 0 of
       Ok (val,col) | col == ncols -> return val
                    | otherwise -> do
                        vals <- forM' 0 (ncols-1) $ \c -> do
                            let v = getvalue result row c
                            return ( typenames V.! c
                                   , v )
                        throw (ConversionFailed
                         (show ncols ++ " values: " ++ show vals)
                         (show col ++ " slots in target type")
                         "mismatch between number of columns to \
                         \convert and number in target type")
       Errors []  -> throwIO $ ConversionFailed "" "" "unknown error"
       Errors [x] -> throwIO x
       Errors xs  -> throwIO $ ManyErrors xs
