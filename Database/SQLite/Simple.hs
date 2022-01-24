{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables, GADTs #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
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

    -- *** Positional parameters
    -- $substpos

    -- *** Named parameters
    -- $substnamed

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

    -- *** Conversion to/from UTCTime
    -- $utctime

    Query(..)
  , Connection(..)
  , ToRow(..)
  , FromRow(..)
  , Only(..)
  , (:.)(..)
  , Base.SQLData(..)
  , Statement(..)
  , ColumnIndex(..)
  , NamedParam(..)
    -- * Connections
  , open
  , close
  , withConnection
  , setTrace
    -- * Queries that return results
  , query
  , query_
  , queryWith
  , queryWith_
  , queryNamed
  , lastInsertRowId
  , changes
  , totalChanges
    -- * Queries that stream results
  , fold
  , fold_
  , foldNamed
    -- * Statements that do not return results
  , execute
  , execute_
  , executeMany
  , executeNamed
  , field
    -- * Transactions
  , withTransaction
  , withImmediateTransaction
  , withExclusiveTransaction
  , withSavepoint
    -- * Low-level statement API for stream access and prepared statements
  , openStatement
  , closeStatement
  , withStatement
  , bind
  , bindNamed
  , reset
  , columnName
  , columnCount
  , withBind
  , nextRow
    -- ** Exceptions
  , FormatError(..)
  , ResultError(..)
  , Base.SQLError(..)
  , Base.Error(..)
  ) where

import           Control.Exception
import           Control.Monad (void, when, forM_)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Int (Int64)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)
import           Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base
import qualified Database.SQLite3.Direct as BaseD


import           Database.SQLite.Simple.FromField (ResultError(..))
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField (ToField(..))
import           Database.SQLite.Simple.ToRow (ToRow(..))

-- | An SQLite prepared statement.
newtype Statement = Statement { unStatement :: Base.Statement }

-- | Index of a column in a result set. Column indices start from 0.
newtype ColumnIndex = ColumnIndex BaseD.ColumnIndex
    deriving (Eq, Ord, Enum, Num, Real, Integral)

data NamedParam where
    (:=) :: (ToField v) => T.Text -> v -> NamedParam

data TransactionType
  = Deferred
  | Immediate
  | Exclusive
  | Savepoint T.Text

infixr 3 :=

instance Show NamedParam where
  show (k := v) = show (k, toField v)

-- | Exception thrown if a 'Query' was malformed.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery   :: Query
    , fmtParams  :: [String]
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
open fname = Connection <$> Base.open (T.pack fname) <*> newIORef 0

-- | Close a database connection.
close :: Connection -> IO ()
close = Base.close . connectionHandle

-- | Opens a database connection, executes an action using this connection, and
-- closes the connection, even in the presence of exceptions.
withConnection :: String -> (Connection -> IO a) -> IO a
withConnection connString = bracket (open connString) close

unUtf8 :: BaseD.Utf8 -> T.Text
unUtf8 (BaseD.Utf8 bs) = TE.decodeUtf8 bs

-- | <http://www.sqlite.org/c3ref/profile.html>
--
-- Enable/disable tracing of SQL execution.  Tracing can be disabled
-- by setting 'Nothing' as the logger callback.
--
-- Warning: If the logger callback throws an exception, your whole
-- program may crash.  Enable only for debugging!
setTrace :: Connection -> Maybe (T.Text -> IO ()) -> IO ()
setTrace conn logger =
  BaseD.setTrace (connectionHandle conn) (fmap (\lf -> lf . unUtf8) logger)

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
        Nothing -> return $! ()

-- | Binds named parameters to a prepared statement.
bindNamed :: Statement -> [NamedParam] -> IO ()
bindNamed (Statement stmt) params = do
  stmtParamCount <- Base.bindParameterCount stmt
  when (length params /= fromIntegral stmtParamCount) $ throwColumnMismatch stmtParamCount
  bind stmt params
  where
    bind stmt params =
      mapM_ (\(n := v) -> do
              idx <- BaseD.bindParameterIndex stmt (BaseD.Utf8 . TE.encodeUtf8 $ n)
              case idx of
                Just i ->
                  Base.bindSQLData stmt i (toField v)
                Nothing -> do
                  templ <- getQuery stmt
                  fmtError ("Unknown named parameter '" ++ T.unpack n ++ "'")
                    templ params)
            params

    throwColumnMismatch nParams = do
      templ <- getQuery stmt
      fmtError ("SQL query contains " ++ show nParams ++ " params, but " ++
                show (length params) ++ " arguments given") templ params

-- | Resets a statement. This does not reset bound parameters, if any, but
-- allows the statement to be reexecuted again by invoking 'nextRow'.
reset :: Statement -> IO ()
reset (Statement stmt) = Base.reset stmt

-- | Return the name of a a particular column in the result set of a
-- 'Statement'.  Throws an 'ArrayException' if the colum index is out
-- of bounds.
--
-- <http://www.sqlite.org/c3ref/column_name.html>
columnName :: Statement -> ColumnIndex -> IO T.Text
columnName (Statement stmt) (ColumnIndex n) = BaseD.columnName stmt n >>= takeUtf8
  where
    takeUtf8 (Just s) = return $ unUtf8 s
    takeUtf8 Nothing  =
      throwIO (IndexOutOfBounds ("Column index " ++ show n ++ " out of bounds"))

-- | Return number of columns in the query
columnCount :: Statement -> IO ColumnIndex
columnCount (Statement stmt) = ColumnIndex <$> BaseD.columnCount stmt

-- | Binds parameters to a prepared statement, and 'reset's the statement when
-- the callback completes, even in the presence of exceptions.
--
-- Use 'withBind' to reuse prepared statements.  Because it 'reset's the
-- statement /after/ each usage, it avoids a pitfall involving implicit
-- transactions.  SQLite creates an implicit transaction if you don't say
-- @BEGIN@ explicitly, and does not commit it until all active statements are
-- finished with either 'reset' or 'closeStatement'.
withBind :: (ToRow params) => Statement -> params -> IO a -> IO a
withBind stmt params io = do
  bind stmt params
  io `finally` reset stmt

-- | Opens a prepared statement. A prepared statement must always be closed with
-- a corresponding call to 'closeStatement' before closing the connection. Use
-- 'nextRow' to iterate on the values returned. Once 'nextRow' returns
-- 'Nothing', you need to invoke 'reset' before reexecuting the statement again
-- with 'nextRow'.
openStatement :: Connection -> Query -> IO Statement
openStatement conn (Query t) = do
  stmt <- Base.prepare (connectionHandle conn) t
  return $ Statement stmt

-- | Closes a prepared statement.
closeStatement :: Statement -> IO ()
closeStatement (Statement stmt) = Base.finalize stmt

-- | Opens a prepared statement, executes an action using this statement, and
-- closes the statement, even in the presence of exceptions.
withStatement :: Connection -> Query -> (Statement -> IO a) -> IO a
withStatement conn query = bracket (openStatement conn query) closeStatement

-- A version of 'withStatement' which binds parameters.
withStatementParams :: (ToRow params)
                       => Connection
                       -> Query
                       -> params
                       -> (Statement -> IO a)
                       -> IO a
withStatementParams conn template params action =
  withStatement conn template $ \stmt ->
    -- Don't use withBind here, there is no need to reset the parameters since
    -- we're destroying the statement
    bind stmt (toRow params) >> action stmt

-- A version of 'withStatement' which binds named parameters.
withStatementNamedParams :: Connection
                         -> Query
                         -> [NamedParam]
                         -> (Statement -> IO a)
                         -> IO a
withStatementNamedParams conn template namedParams action =
  withStatement conn template $ \stmt -> bindNamed stmt namedParams >> action stmt

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Query -> q -> IO ()
execute conn template qs =
  withStatementParams conn template qs $ \(Statement stmt) ->
    void . Base.step $ stmt

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
executeMany :: ToRow q => Connection -> Query -> [q] -> IO ()
executeMany conn template paramRows = withStatement conn template $ \stmt -> do
  let Statement stmt' = stmt
  forM_ paramRows $ \params ->
    withBind stmt params
      (void . Base.step $ stmt')


doFoldToList :: RowParser row -> Statement -> IO [row]
doFoldToList fromRow_ stmt =
  fmap reverse $ doFold fromRow_ stmt [] (\acc e -> return (e : acc))

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
query = queryWith fromRow

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ = queryWith_ fromRow

-- | A version of 'query' that takes an explicit 'RowParser'.
queryWith :: (ToRow q) => RowParser r -> Connection -> Query -> q -> IO [r]
queryWith fromRow_ conn templ qs =
  withStatementParams conn templ qs $ \stmt -> doFoldToList fromRow_ stmt

-- | A version of 'query' that does not perform query substitution and
-- takes an explicit 'RowParser'.
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryWith_ fromRow_ conn query =
  withStatement conn query (doFoldToList fromRow_)

-- | A version of 'query' where the query parameters (placeholders)
-- are named.
--
-- Example:
--
-- @
-- r \<- 'queryNamed' c \"SELECT * FROM posts WHERE id=:id AND date>=:date\" [\":id\" ':=' postId, \":date\" ':=' afterDate]
-- @
queryNamed :: (FromRow r) => Connection -> Query -> [NamedParam] -> IO [r]
queryNamed conn templ params =
  withStatementNamedParams conn templ params $ \stmt -> doFoldToList fromRow stmt

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO ()
execute_ conn template =
  withStatement conn template $ \(Statement stmt) ->
    void $ Base.step stmt

-- | A version of 'execute' where the query parameters (placeholders)
-- are named.
executeNamed :: Connection -> Query -> [NamedParam] -> IO ()
executeNamed conn template params =
  withStatementNamedParams conn template params $ \(Statement stmt) ->
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
  withStatementParams conn query params $ \stmt ->
    doFold fromRow stmt initalState action

-- | A version of 'fold' which does not perform parameter substitution.
fold_ :: ( FromRow row )
        => Connection
        -> Query
        -> a
        -> (a -> row -> IO a)
        -> IO a
fold_ conn query initalState action =
  withStatement conn query $ \stmt ->
    doFold fromRow stmt initalState action

-- | A version of 'fold' where the query parameters (placeholders) are
-- named.
foldNamed :: ( FromRow row )
          => Connection
          -> Query
          -> [NamedParam]
          -> a
          -> (a -> row -> IO a)
          -> IO a
foldNamed conn query params initalState action =
  withStatementNamedParams conn query params $ \stmt ->
    doFold fromRow stmt initalState action

doFold :: RowParser row -> Statement ->  a -> (a -> row -> IO a) -> IO a
doFold fromRow_ stmt initState action =
  loop initState
  where
    loop val = do
      maybeNextRow <- nextRowWith fromRow_ stmt
      case maybeNextRow of
        Just row  -> do
          val' <- action val row
          val' `seq` loop val'
        Nothing   -> return val

-- | Extracts the next row from the prepared statement.
nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow = nextRowWith fromRow

nextRowWith :: RowParser r -> Statement -> IO (Maybe r)
nextRowWith fromRow_ (Statement stmt) = do
  statRes <- Base.step stmt
  case statRes of
    Base.Row -> do
      rowRes <- Base.columns stmt
      let nCols = length rowRes
      row <- convertRow fromRow_ rowRes nCols
      return $ Just row
    Base.Done -> return Nothing

convertRow :: RowParser r -> [Base.SQLData] -> Int -> IO r
convertRow fromRow_ rowRes ncols = do
  let rw = RowParseRO ncols
  case runStateT (runReaderT (unRP fromRow_) rw) (0, rowRes) of
    Ok (val,(col,_))
       | col == ncols -> return val
       | otherwise -> errorColumnMismatch (ColumnOutOfBounds col)
    Errors []  -> throwIO $ ConversionFailed "" "" "unknown error"
    Errors [x] ->
      throw x `Control.Exception.catch` (\e -> errorColumnMismatch (e :: ColumnOutOfBounds))
    Errors xs  -> throwIO $ ManyErrors xs
  where
    errorColumnMismatch :: ColumnOutOfBounds -> IO r
    errorColumnMismatch (ColumnOutOfBounds c) = do
      let vals = map (\f -> (gettypename f, ellipsis f)) rowRes
      throwIO (ConversionFailed
               (show ncols ++ " values: " ++ show vals)
               ("at least " ++ show c ++ " slots in target type")
               "mismatch between number of columns to convert and number in target type")

    ellipsis :: Base.SQLData -> T.Text
    ellipsis sql
      | T.length bs > 20 = T.take 15 bs `T.append` "[...]"
      | otherwise        = bs
      where
        bs = T.pack $ show sql

withTransactionPrivate :: Connection -> IO a -> TransactionType -> IO a
withTransactionPrivate conn action ttype =
  mask $ \restore -> do
    begin
    r <- restore action `onException` rollback
    commit
    return r
  where
    begin    = execute_ conn $ case ttype of
                 Deferred  -> "BEGIN TRANSACTION"
                 Immediate -> "BEGIN IMMEDIATE TRANSACTION"
                 Exclusive -> "BEGIN EXCLUSIVE TRANSACTION"
                 Savepoint name -> Query $ "SAVEPOINT '" <> name <> "'"
    commit   = execute_ conn $ case ttype of
                 Savepoint name -> Query $ "RELEASE '" <> name <> "'"
                 _ -> "COMMIT TRANSACTION"
    rollback = execute_ conn $ case ttype of
                 Savepoint name -> Query $ "ROLLBACK TO '" <> name <> "'"
                 _ -> "ROLLBACK TRANSACTION"


-- | Run an IO action inside a SQL transaction started with @BEGIN IMMEDIATE
-- TRANSACTION@, which immediately blocks all other database connections from
-- writing.  The default SQLite3 @BEGIN TRANSACTION@ does not acquire the write
-- lock on @BEGIN@ nor on @SELECT@ but waits until you try to change data.  If
-- the action throws any kind of an exception, the transaction will be rolled
-- back with @ROLLBACK TRANSACTION@.  Otherwise the results are committed with
-- @COMMIT TRANSACTION@.
withImmediateTransaction :: Connection -> IO a -> IO a
withImmediateTransaction conn action =
  withTransactionPrivate conn action Immediate

-- | Run an IO action inside a SQL transaction started with @BEGIN EXCLUSIVE
-- TRANSACTION@, which immediately blocks all other database connections from
-- writing, and other connections from reading (exception: read_uncommitted
-- connections are allowed to read.) If the action throws any kind of an
-- exception, the transaction will be rolled back with @ROLLBACK TRANSACTION@.
-- Otherwise the results are committed with @COMMIT TRANSACTION@.
withExclusiveTransaction :: Connection -> IO a -> IO a
withExclusiveTransaction conn action =
  withTransactionPrivate conn action Exclusive

-- | Returns the rowid of the most recent successful INSERT on the
-- given database connection.
--
-- See also <http://www.sqlite.org/c3ref/last_insert_rowid.html>.
lastInsertRowId :: Connection -> IO Int64
lastInsertRowId = BaseD.lastInsertRowId . connectionHandle

-- | <http://www.sqlite.org/c3ref/changes.html>
--
-- Return the number of rows that were changed, inserted, or deleted
-- by the most recent @INSERT@, @DELETE@, or @UPDATE@ statement.
changes :: Connection -> IO Int
changes = BaseD.changes . connectionHandle

-- | <http://www.sqlite.org/c3ref/total_changes.html>
--
-- Return the total number of row changes caused by @INSERT@, @DELETE@,
-- or @UPDATE@ statements since the 'Database' was opened.
totalChanges :: Connection -> IO Int
totalChanges = BaseD.totalChanges . connectionHandle

-- | Run an IO action inside a SQL transaction started with @BEGIN
-- TRANSACTION@.  If the action throws any kind of an exception, the
-- transaction will be rolled back with @ROLLBACK TRANSACTION@.
-- Otherwise the results are committed with @COMMIT TRANSACTION@.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
  withTransactionPrivate conn action Deferred

-- | Run an IO action inside an SQLite @SAVEPOINT@. If the action throws any
-- kind of an exception, the transaction will be rolled back to the savepoint
-- with @ROLLBACK TO@. Otherwise the results are released to the outer
-- transaction if any with @RELEASE@.
--
-- See <https://sqlite.org/lang_savepoint.html> for a full description of
-- savepoint semantics.
withSavepoint :: Connection -> IO a -> IO a
withSavepoint conn action = do
  n <- atomicModifyIORef' (connectionTempNameCounter conn) $ \n -> (n + 1, n)
  withTransactionPrivate conn action $
    Savepoint $ "sqlite_simple_savepoint_" <> T.pack (show n)

fmtError :: Show v => String -> Query -> [v] -> a
fmtError msg q xs =
  throw FormatError {
      fmtMessage  = msg
    , fmtQuery    = q
    , fmtParams   = map show xs
    }

getQuery :: Base.Statement -> IO Query
getQuery stmt =
  toQuery <$> BaseD.statementSql stmt
  where
    toQuery =
      Query . maybe "no query string" (\(BaseD.Utf8 s) -> TE.decodeUtf8 s)

-- $use
-- An example that creates a table 'test', inserts a couple of rows
-- and proceeds to showcase how to update or delete rows.  This
-- example also demonstrates the use of 'lastInsertRowId' (how to
-- refer to a previously inserted row) and 'executeNamed' (an easier
-- to maintain form of query parameter naming).
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import           Control.Applicative
-- >import qualified Data.Text as T
-- >import           Database.SQLite.Simple
-- >import           Database.SQLite.Simple.FromRow
-- >
-- >data TestField = TestField Int T.Text deriving (Show)
-- >
-- >instance FromRow TestField where
-- >  fromRow = TestField <$> field <*> field
-- >
-- >instance ToRow TestField where
-- >  toRow (TestField id_ str) = toRow (id_, str)
-- >
-- >main :: IO ()
-- >main = do
-- >  conn <- open "test.db"
-- >  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
-- >  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
-- >  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
-- >  rowId <- lastInsertRowId conn
-- >  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
-- >  r <- query_ conn "SELECT * from test" :: IO [TestField]
-- >  mapM_ print r
-- >  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
-- >  close conn

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
-- >   [[x]] <- query_ conn "select 2 + 2"
-- >   print x
--
-- A 'Query' value does not represent the actual query that will be
-- executed, but is a template for constructing the final query.

-- $subst
--
-- Since applications need to be able to construct queries with
-- parameters that change, this library uses SQLite's parameter
-- binding query substitution capability.
--
-- This library restricts parameter substitution to work only with
-- named parameters and positional arguments with the \"@?@\" syntax.
-- The API does not support for mixing these two types of bindings.
-- Unsupported parameters will be rejected and a 'FormatError' will be
-- thrown.
--
-- You should always use parameter substitution instead of inlining
-- your dynamic parameters into your queries with messy string
-- concatenation.  SQLite will automatically quote and escape your
-- data into these placeholder parameters; this defeats the single
-- most common injection vector for malicious data.

-- $substpos
--
-- The 'Query' template accepted by 'query', 'execute' and 'fold' can
-- contain any number of \"@?@\" characters.  Both 'query' and
-- 'execute' accept a third argument, typically a tuple. When the
-- query executes, the first \"@?@\" in the template will be replaced
-- with the first element of the tuple, the second \"@?@\" with the
-- second element, and so on.  This substitution happens inside the
-- native SQLite implementation.
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

-- $substnamed
--
-- Named parameters are accepted by 'queryNamed', 'executeNamed' and
-- 'foldNamed'.  These functions take a list of 'NamedParam's which
-- are key-value pairs binding a value to an argument name.  As is the
-- case with \"@?@\" parameters, named parameters are automatically
-- escaped by the SQLite library.  The parameter names are prefixed
-- with either @:@ or @\@@, e.g. @:foo@ or @\@foo@.
--
-- Example:
--
-- @
-- r \<- 'queryNamed' c \"SELECT id,text FROM posts WHERE id = :id AND date >= :date\" [\":id\" ':=' postId, \":date\" ':=' afterDate]
-- @
--
-- Note that you can mix different value types in the same list.
-- E.g., the following is perfectly legal:
--
-- @
-- [\":id\" ':=' (3 :: Int), \":str\" ':=' (\"foo\" :: String)]
-- @
--
-- The parameter name (or key) in the 'NamedParam' must match exactly
-- the name written in the SQL query.  E.g., if you used @:foo@ in
-- your SQL statement, you need to use @\":foo\"@ as the parameter
-- key, not @\"foo\"@.  Some libraries like Python's sqlite3
-- automatically drop the @:@ character from the name.

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
--
-- Or you can use named parameters which do not have this restriction.

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

-- $utctime
--
-- SQLite's datetime allows for multiple string representations of UTC
-- time.  The following formats are supported for reading SQLite times
-- into Haskell UTCTime values:
--
-- * YYYY-MM-DD HH:MM
--
-- * YYYY-MM-DD HH:MM:SS
--
-- * YYYY-MM-DD HH:MM:SS.SSS
--
-- * YYYY-MM-DDTHH:MM
--
-- * YYYY-MM-DDTHH:MM:SS
--
-- * YYYY-MM-DDTHH:MM:SS.SSS
--
-- The above may also be optionally followed by a timezone indicator
-- of the form \"[+-]HH:MM\" or just \"Z\".
--
-- When Haskell UTCTime values are converted into SQLite values (e.g.,
-- parameters for a 'query'), the following format is used:
--
-- * YYYY-MM-DD HH:MM:SS.SSS
--
-- The last \".SSS\" subsecond part is dropped if it's zero.  No
-- timezone indicator is used when converting from a UTCTime value
-- into an SQLite string.  SQLite assumes all datetimes are in UTC
-- time.
--
-- The parser and printers are implemented in <Database-SQLite-Simple-Time.html Database.SQLite.Simple.Time>.
--
-- Read more about SQLite's time strings in <http://sqlite.org/lang_datefunc.html>
