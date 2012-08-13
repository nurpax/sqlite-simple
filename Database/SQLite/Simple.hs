{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
------------------------------------------------------------------------------

module Database.SQLite.Simple (
    -- * Examples of use
    -- $use
    open
  , close
  , query
  , query_
  , execute
  , execute_
  , field
  , Query
  , Connection
  , ToRow
  , FromRow
  , In(..)
  , Binary(..)
  , Only(..)
  , (:.)(..)
    -- ** Exceptions
  , FormatError(fmtMessage, fmtQuery, fmtParams)
  , ResultError(errSQLType, errHaskellType, errMessage)
  ) where

import           Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import           Blaze.ByteString.Builder.Char8 (fromChar)
import           Control.Applicative
import           Control.Exception
  ( Exception, onException, throw, throwIO, finally, bracket )
import           Control.Monad (void, when)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.ByteString (ByteString)
import           Data.List (intersperse)
import           Data.Monoid (mappend, mconcat)
import           Data.Typeable (Typeable)
import           Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base
import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import           Database.SQLite.Simple.FromField (ResultError(..))
import           Database.SQLite.Simple.FromRow (FromRow(..))
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.ToRow (ToRow(..))
import           Database.SQLite.Simple.Types(
  Binary(..), In(..), Only(..), Query(..), (:.)(..))
import           Database.SQLite.Simple.FromRow

{- $use
Create a test database by copy&pasting the below snippet to your
shell:

@
sqlite3 test.db \"CREATE TABLE test (id INTEGER PRIMARY KEY, str text);\\
INSERT INTO test (str) VALUES ('test string');\"
@

..and access it from Haskell:

@
import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField \<$\> field \<*\> field

main :: IO ()
main = do
  conn <- open \"test.db\"
  r <- query_ conn \"SELECT * from test\" :: IO [TestField]
  mapM_ print r
  close conn
@
-}

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

-- | Open a database connection to a given file.  Will throw an
-- exception if it cannot connect.
--
-- Every 'open' must be closed with a call to 'close'.
open :: String -> IO Connection
open fname = Connection <$> Base.open fname

-- | Close a database connection.
close :: Connection -> IO ()
close (Connection c) = Base.close c

withBind :: Query -> Base.Statement -> [Base.SQLData] -> IO r -> IO r
withBind templ stmt qp action = do
  stmtParamCount <- Base.bindParameterCount stmt
  when (length qp /= stmtParamCount) (throwColumnMismatch qp stmtParamCount)
  mapM_ errorCheckParamName [1..stmtParamCount]
  Base.bind stmt qp
  action
  where
    throwColumnMismatch qp nParams =
      fmtError ("SQL query contains " ++ show nParams ++ " params, but " ++
                show (length qp) ++ " arguments given") templ qp
    errorCheckParamName paramNdx = do
      name <- Base.bindParameterName stmt paramNdx
      case name of
        Just n ->
          fmtError ("Only unnamed '?' query parameters are accepted, '"++n++"' given")
                    templ qp
        Nothing -> return ()

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Query -> q -> IO ()
execute (Connection c) template@(Query t) qs = do
  bracket (Base.prepare c (utf8ToString t)) Base.finalize go
  where
    go stmt = withBind template stmt (toRow qs) (void $ Base.step stmt)

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
query :: (ToRow q, FromRow r)
         => Connection -> Query -> q -> IO [r]
query (Connection conn) templ@(Query t) qs = do
  bracket (Base.prepare conn (utf8ToString t)) Base.finalize go
  where
    go stmt = withBind templ stmt (toRow qs) (stepStmt stmt >>= finishQuery)

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn (Query que) = do
  result <- exec conn que
  finishQuery result

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO ()
execute_ (Connection conn) (Query que) =
  bracket (Base.prepare conn (utf8ToString que)) Base.finalize go
    where
      go stmt = void $ Base.step stmt


finishQuery :: (FromRow r) => Result -> IO [r]
finishQuery rows =
  mapM doRow $ zip rows [0..]
    where
      doRow (rowRes, rowNdx) = do
        let rw = Row rowNdx rowRes
        case runStateT (runReaderT (unRP fromRow) rw) 0 of
          Ok (val,col) | col == ncols -> return val
                       | otherwise -> do
                           let vals = map (\f -> (gettypename f, f)) rowRes
                           throw (ConversionFailed
                             (show ncols ++ " values: " ++ show vals)
                             (show col ++ " slots in target type")
                             "mismatch between number of columns to \
                             \convert and number in target type")
          Errors []  -> throwIO $ ConversionFailed "" "" "unknown error"
          Errors [x] -> throwIO x
          Errors xs  -> throwIO $ ManyErrors xs

      ncols = length . head $ rows

fmtError :: String -> Query -> [Base.SQLData] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map (B.pack . show) xs
                    }
