{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

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
    -- ** Examples of use
    -- $use

    -- ** The Query type
    -- $querytype

    -- ** Parameter substitution
    -- $subst

    -- *** Type inference
    -- $inference

    -- ** Substituting a single parameter
    -- $only_param

    -- * Extracting results
    -- $result

    -- ** Handling null values
    -- $null

    -- ** Type conversions
    -- $types
    Query(..)
  , Connection
  , ToRow(..)
  , FromRow(..)
  , Only(..)
  , (:.)(..)
  , Base.SQLData(..)
  , Statement
    -- * Connections
  , open
  , close
  , withConnection
  , setTrace
    -- * Queries that return results
  , query
  , query_
  , lastInsertRowId
    -- * Statements that do not return results
  , execute
  , execute_
  , field
  , fold
  , fold_
    -- * Low-level statement API for stream access and prepared statements
  , openStatement
  , closeStatement
  , withStatement
  , bind
  , reset
  , withBind
  , nextRow
    -- ** Exceptions
  , FormatError(fmtMessage, fmtQuery, fmtParams)
  , ResultError(errSQLType, errHaskellType, errMessage)
  ) where

import           Control.Applicative
import           Control.Exception (Exception, throw, throwIO, bracket)
import           Control.Monad (void, when)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)
import           Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base
import qualified Database.SQLite3.Direct as BaseD


import           Database.SQLite.Simple.FromField (ResultError(..))
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToRow (ToRow(..))
import           Database.SQLite.Simple.FromRow

-- | An SQLite prepared statement.
newtype Statement = Statement Base.Statement

-- | Exception thrown if a 'Query' was malformed.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [String]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

-- | Open a database connection to a given file.  Will throw an
-- exception if it cannot connect.
--
-- Every 'open' must be closed with a call to 'close'.
--
-- If you specify \":memory:\" or an empty string as the input filename,
-- then a private, temporary in-memory database is created for the
-- connection.  This database will vanish when you close the
-- connection.
open :: String -> IO Connection
open fname = Connection <$> Base.open (T.pack fname)

-- | Close a database connection.
close :: Connection -> IO ()
close (Connection c) = Base.close c

-- | Opens a database connection, executes an action using this connection, and
-- closes the connection, even in the presence of exceptions.
withConnection :: String -> (Connection -> IO a) -> IO a
withConnection connString = bracket (open connString) close

-- | <http://www.sqlite.org/c3ref/profile.html>
--
-- Enable/disable tracing of SQL execution.  Tracing can be disabled
-- by setting 'Nothing' as the logger callback.
--
-- Warning: If the logger callback throws an exception, your whole
-- program may crash.  Enable only for debugging!
setTrace :: Connection -> Maybe (T.Text -> IO ()) -> IO ()
setTrace (Connection db) logger =
  BaseD.setTrace db (fmap (\lf -> lf . unUtf8) logger)
  where
    unUtf8 (BaseD.Utf8 bs) = TE.decodeUtf8 bs

-- | Binds parameters to a prepared statement. Once 'nextRow' returns 'Nothing',
-- the statement must be reset with the 'reset' function before it can be
-- executed again by calling 'nextRow'.
bind :: (ToRow params) => Statement -> params -> IO ()
bind (Statement stmt) params = do
  let qp = toRow params
  stmtParamCount <- Base.bindParameterCount stmt
  when (length qp /= fromIntegral stmtParamCount) (throwColumnMismatch qp stmtParamCount)
  mapM_ (errorCheckParamName qp) [1..stmtParamCount]
  Base.bind stmt qp
  where
    throwColumnMismatch qp nParams = do
      templ <- getQuery stmt
      fmtError ("SQL query contains " ++ show nParams ++ " params, but " ++
                show (length qp) ++ " arguments given") templ qp
    errorCheckParamName qp paramNdx = do
      templ <- getQuery stmt
      name <- Base.bindParameterName stmt paramNdx
      case name of
        Just n ->
          fmtError ("Only unnamed '?' query parameters are accepted, '"++T.unpack n++"' given")
                    templ qp
        Nothing -> return ()

-- | Resets a statement. This does not reset bound parameters, if any, but
-- allows the statement to be reexecuted again by invoking 'nextRow'.
reset :: Statement -> IO ()
reset (Statement stmt) = Base.reset stmt

-- | Binds parameters to a prepared statement and then resets them, even in the
-- presence of exceptions.
withBind :: (ToRow params) => Statement -> params -> IO a -> IO a
withBind stmt params io =
  bracket (bind stmt params >> return stmt) reset (const io)

-- | Opens a prepared statement. A prepared statement must always be closed with
-- a corresponding call to 'closeStatement' before closing the connection. Use
-- 'nextRow' to iterate on the values returned. Once 'nextRow' returns
-- 'Nothing', you need to invoke 'reset' before reexecuting the statement again
-- with 'nextRow'.
openStatement :: Connection -> Query -> IO Statement
openStatement (Connection c) (Query t) = do
  stmt <- Base.prepare c t
  return $ Statement stmt

-- | Closes a prepared statement.
closeStatement :: Statement -> IO ()
closeStatement (Statement stmt) = Base.finalize stmt

-- | Opens a prepared statement, executes an action using this statement, and
-- closes the statement, even in the presence of exceptions.
withStatement :: Connection -> Query -> (Statement -> IO a) -> IO a
withStatement conn query = bracket (openStatement conn query) closeStatement

-- A version of 'withStatement' which binds parameters.
withStatementP :: (ToRow params) => Connection -> Query -> params -> (Statement -> IO a) -> IO a
withStatementP conn template params action =
  withStatement conn template $ \stmt ->
    -- Don't use withBind here, there is no need to reset the parameters since
    -- we're destroying the statement
    bind stmt (toRow params) >> action stmt

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Query -> q -> IO ()
execute conn template qs =
  withStatementP conn template qs $ \(Statement stmt) ->
    void . Base.step $ stmt


doFoldToList :: (FromRow row) => Statement -> IO [row]
doFoldToList stmt =
  fmap reverse $ doFold stmt [] (\acc e -> return (e : acc))

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
-- * 'ResultError': result conversion failed.
query :: (ToRow q, FromRow r)
         => Connection -> Query -> q -> IO [r]
query conn templ qs =
  withStatementP conn templ qs $ \stmt ->
    doFoldToList stmt

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn query =
  withStatement conn query doFoldToList

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO ()
execute_ conn template =
  withStatement conn template $ \(Statement stmt) ->
    void $ Base.step stmt

-- | Perform a @SELECT@ or other SQL query that is expected to return results.
-- Results are converted and fed into the 'action' callback as they are being
-- retrieved from the database.
--
-- This allows gives the possibility of processing results in constant space
-- (for instance writing them to disk).
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
fold :: ( FromRow row, ToRow params )
        => Connection
        -> Query
        -> params
        -> a
        -> (a -> row -> IO a)
        -> IO a
fold conn query params initalState action =
  withStatementP conn query params $ \stmt ->
    doFold stmt initalState action

-- | A version of 'fold' which does not perform parameter substitution.
fold_ :: ( FromRow row )
        => Connection
        -> Query
        -> a
        -> (a -> row -> IO a)
        -> IO a
fold_ conn query initalState action =
  withStatement conn query $ \stmt ->
    doFold stmt initalState action

doFold :: (FromRow row) => Statement ->  a -> (a -> row -> IO a) -> IO a
doFold stmt initState action =
  loop initState
  where
    loop val = do
      maybeNextRow <- nextRow stmt
      case maybeNextRow of
        Just row  -> do
          val' <- action val row
          loop val'
        Nothing   -> return val

-- | Extracts the next row from the prepared statement.
nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow (Statement stmt) = do
  statRes <- Base.step stmt
  case statRes of
    Base.Row -> do
      rowRes <- Base.columns stmt
      let nCols = length rowRes
      row <- convertRow rowRes nCols
      return $ Just row
    Base.Done -> return Nothing

convertRow :: (FromRow r) => [Base.SQLData] -> Int -> IO r
convertRow rowRes ncols = do
  let rw = Row rowRes
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

-- | Returns the rowid of the most recent successful INSERT on the
-- given database connection.
--
-- See also <http://www.sqlite.org/c3ref/last_insert_rowid.html>.
lastInsertRowId :: Connection -> IO Int64
lastInsertRowId (Connection c) = BaseD.lastInsertRowId c

fmtError :: String -> Query -> [Base.SQLData] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map show xs
                    }

getQuery :: Base.Statement -> IO Query
getQuery stmt =
  toQuery <$> BaseD.statementSql stmt
  where
    toQuery =
      Query . maybe "no query string" (\(BaseD.Utf8 s) -> TE.decodeUtf8 s)

-- $use
-- Create a test database by copy pasting the below snippet to your
-- shell:
--
-- @
-- sqlite3 test.db \"CREATE TABLE test (id INTEGER PRIMARY KEY, str text); \\
-- INSERT INTO test (str) VALUES ('test string');\"
-- @
--
-- ..and access it from Haskell:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Control.Applicative
-- > import Database.SQLite.Simple
-- > import Database.SQLite.Simple.FromRow
-- >
-- > data TestField = TestField Int String deriving (Show)
-- >
-- > instance FromRow TestField where
-- >   fromRow = TestField <$> field <*> field
-- >
-- > main :: IO ()
-- > main = do
-- >   conn <- open "test.db"
-- >   execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
-- >   r <- query_ conn "SELECT * from test" :: IO [TestField]
-- >   mapM_ print r
-- >   close conn

-- $querytype
--
-- SQL-based applications are somewhat notorious for their
-- susceptibility to attacks through the injection of maliciously
-- crafted data. The primary reason for widespread vulnerability to
-- SQL injections is that many applications are sloppy in handling
-- user data when constructing SQL queries.
--
-- This library provides a 'Query' type and a parameter substitution
-- facility to address both ease of use and security.  A 'Query' is a
-- @newtype@-wrapped 'Text'. It intentionally exposes a tiny API that
-- is not compatible with the 'Text' API; this makes it difficult to
-- construct queries from fragments of strings.  The 'query' and
-- 'execute' functions require queries to be of type 'Query'.
--
-- To most easily construct a query, enable GHC's @OverloadedStrings@
-- language extension and write your query as a normal literal string.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.SQLite.Simple
-- >
-- > hello = do
-- >   conn <- open "test.db"
-- >   query conn "select 2 + 2"
--
-- A 'Query' value does not represent the actual query that will be
-- executed, but is a template for constructing the final query.

-- $subst
--
-- Since applications need to be able to construct queries with
-- parameters that change, this library uses SQLite's parameter
-- binding query substitution capability.
--
-- The 'Query' template accepted by 'query' and 'execute' can contain
-- any number of \"@?@\" characters.  Both 'query' and 'execute'
-- accept a third argument, typically a tuple. When constructing the
-- real query to execute, these functions replace the first \"@?@\" in
-- the template with the first element of the tuple, the second
-- \"@?@\" with the second element, and so on. If necessary, each
-- tuple element will be quoted and escaped prior to substitution;
-- this defeats the single most common injection vector for malicious
-- data.
--
-- For example, given the following 'Query' template:
--
-- > select * from user where first_name = ? and age > ?
--
-- And a tuple of this form:
--
-- > ("Boris" :: String, 37 :: Int)
--
-- The query to be executed will look like this after substitution:
--
-- > select * from user where first_name = 'Boris' and age > 37
--
-- If there is a mismatch between the number of \"@?@\" characters in
-- your template and the number of elements in your tuple, a
-- 'FormatError' will be thrown.
--
-- Note that the substitution functions do not attempt to parse or
-- validate your query. It's up to you to write syntactically valid
-- SQL, and to ensure that each \"@?@\" in your query template is
-- matched with the right tuple element.
--
-- This library restricts parameter substitution to work only with
-- \"@?@\" characters.  SQLite natively supports several other
-- syntaxes for binding query parameters.  Unsupported parameters will
-- be rejected and a 'FormatError' will be thrown.

-- $inference
--
-- Automated type inference means that you will often be able to avoid
-- supplying explicit type signatures for the elements of a tuple.
-- However, sometimes the compiler will not be able to infer your
-- types. Consider a case where you write a numeric literal in a
-- parameter tuple:
--
-- > query conn "select ? + ?" (40,2)
--
-- The above query will be rejected by the compiler, because it does
-- not know the specific numeric types of the literals @40@ and @2@.
-- This is easily fixed:
--
-- > query conn "select ? + ?" (40 :: Double, 2 :: Double)
--
-- The same kind of problem can arise with string literals if you have
-- the @OverloadedStrings@ language extension enabled.  Again, just
-- use an explicit type signature if this happens.

-- $only_param
--
-- Haskell lacks a single-element tuple type, so if you have just one
-- value you want substituted into a query, what should you do?
--
-- To represent a single value @val@ as a parameter, write a singleton
-- list @[val]@, use 'Just' @val@, or use 'Only' @val@.
--
-- Here's an example using a singleton list:
--
-- > execute conn "insert into users (first_name) values (?)"
-- >              ["Nuala"]

-- $result
--
-- The 'query' and 'query_' functions return a list of values in the
-- 'FromRow' typeclass. This class performs automatic extraction
-- and type conversion of rows from a query result.
--
-- Here is a simple example of how to extract results:
--
-- > import qualified Data.Text as T
-- >
-- > xs <- query_ conn "select name,age from users"
-- > forM_ xs $ \(name,age) ->
-- >   putStrLn $ T.unpack name ++ " is " ++ show (age :: Int)
--
-- Notice two important details about this code:
--
-- * The number of columns we ask for in the query template must
--   exactly match the number of elements we specify in a row of the
--   result tuple.  If they do not match, a 'ResultError' exception
--   will be thrown.
--
-- * Sometimes, the compiler needs our help in specifying types. It
--   can infer that @name@ must be a 'Text', due to our use of the
--   @unpack@ function. However, we have to tell it the type of @age@,
--   as it has no other information to determine the exact type.

-- $null
--
-- The type of a result tuple will look something like this:
--
-- > (Text, Int, Int)
--
-- Although SQL can accommodate @NULL@ as a value for any of these
-- types, Haskell cannot. If your result contains columns that may be
-- @NULL@, be sure that you use 'Maybe' in those positions of of your
-- tuple.
--
-- > (Text, Maybe Int, Int)
--
-- If 'query' encounters a @NULL@ in a row where the corresponding
-- Haskell type is not 'Maybe', it will throw a 'ResultError'
-- exception.

-- $only_result
--
-- To specify that a query returns a single-column result, use the
-- 'Only' type.
--
-- > xs <- query_ conn "select id from users"
-- > forM_ xs $ \(Only dbid) -> {- ... -}

-- $types
--
-- Conversion of SQL values to Haskell values is somewhat
-- permissive. Here are the rules.
--
-- * For numeric types, any Haskell type that can accurately represent
--   an SQLite INTEGER is considered \"compatible\".
--
-- * If a numeric incompatibility is found, 'query' will throw a
--   'ResultError'.
--
-- * SQLite's TEXT type is always encoded in UTF-8.  Thus any text
--   data coming from an SQLite database should always be compatible
--   with Haskell 'String' and 'Text' types.
--
-- * SQLite's BLOB type will only be conversible to a Haskell
--   'ByteString'.
--
-- You can extend conversion support to your own types be adding your
-- own 'FromField' / 'ToField' instances.
