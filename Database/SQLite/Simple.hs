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
  , executeMany
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
    -- * Helper functions
  , formatMany
  , formatQuery
  ) where

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Applicative
import Control.Exception
  ( Exception, onException, throw, throwIO, finally, bracket )
import Control.Monad (void)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Monoid (mappend, mconcat)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base
import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import Database.SQLite.Simple.FromField (ResultError(..))
import Database.SQLite.Simple.FromRow (FromRow(..))
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField (Action(..), inQuotes)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.Types(
  Binary(..), In(..), Only(..), Query(..), (:.)(..))

import Database.SQLite.Simple.FromRow

{- $use
Create a test database by copy&pasting the below snippet to your
shell:

@
sqlite3 test.db \"CREATE TABLE test (id INTEGER PRIMARY KEY, str text);\\
INSERT INTO test (str) VALUES ('test string');\"
@

..and access it from Haskell:

@
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

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

-- | Format a query string.
--
-- This function is exposed to help with debugging and logging. Do not
-- use it to prepare queries for execution.
--
-- String parameters are escaped according to the character set in use
-- on the 'Connection'.
--
-- Throws 'FormatError' if the query string could not be formatted
-- correctly.
formatQuery :: ToRow q => Connection -> Query -> q -> IO ByteString
formatQuery conn q@(Query template) qs
    | null xs && '?' `B.notElem` template = return template
    | otherwise = toByteString <$> buildQuery conn q template xs
  where xs = toRow qs

-- | Format a query string with a variable number of rows.
--
-- This function is exposed to help with debugging and logging. Do not
-- use it to prepare queries for execution.
--
-- The query string must contain exactly one substitution group,
-- identified by the SQL keyword \"@VALUES@\" (case insensitive)
-- followed by an \"@(@\" character, a series of one or more \"@?@\"
-- characters separated by commas, and a \"@)@\" character. White
-- space in a substitution group is permitted.
--
-- Throws 'FormatError' if the query string could not be formatted
-- correctly.
formatMany :: (ToRow q) => Connection -> Query -> [q] -> IO ByteString
formatMany _ q [] = fmtError "no rows supplied" q []
formatMany conn q@(Query template) qs = do
  case parseTemplate template of
    Just (before, qbits, after) -> do
      bs <- mapM (buildQuery conn q qbits . toRow) qs
      return . toByteString . mconcat $ fromByteString before :
                                        intersperse (fromChar ',') bs ++
                                        [fromByteString after]
    Nothing -> fmtError "syntax error in query template for executeMany" q []

-- Split the input string into three pieces, @before@, @qbits@, and @after@,
-- following this grammar:
--
-- start: ^ before qbits after $
--     before: ([^?]* [^?\w])? 'VALUES' \s*
--     qbits:  '(' \s* '?' \s* (',' \s* '?' \s*)* ')'
--     after:  [^?]*
--
-- \s: [ \t\n\r\f]
-- \w: [A-Z] | [a-z] | [\x80-\xFF] | '_' | '$' | [0-9]
--
-- This would be much more concise with some sort of regex engine.
-- 'formatMany' used to use pcre-light instead of this hand-written parser,
-- but pcre is a hassle to install on Windows.
parseTemplate :: ByteString -> Maybe (ByteString, ByteString, ByteString)
parseTemplate template =
    -- Convert input string to uppercase, to facilitate searching.
    search $ B.map toUpper_ascii template
  where
    -- Search for the next occurrence of "VALUES"
    search bs =
        case B.breakSubstring "VALUES" bs of
            (x, y)
                -- If "VALUES" is not present in the string, or any '?' characters
                -- were encountered prior to it, fail.
                | B.null y || ('?' `B.elem` x)
               -> Nothing

                -- If "VALUES" is preceded by an identifier character (a.k.a. \w),
                -- try the next occurrence.
                | not (B.null x) && isIdent (B.last x)
               -> search $ B.drop 6 y

                -- Otherwise, we have a legitimate "VALUES" token.
                | otherwise
               -> parseQueryBits $ skipSpace $ B.drop 6 y

    -- Parse '(' \s* '?' \s* .  If this doesn't match
    -- (and we don't consume a '?'), look for another "VALUES".
    --
    -- qb points to the open paren (if present), meaning it points to the
    -- beginning of the "qbits" production described above.  This is why we
    -- pass it down to finishQueryBits.
    parseQueryBits qb
        | Just ('(', skipSpace -> bs1) <- B.uncons qb
        , Just ('?', skipSpace -> bs2) <- B.uncons bs1
        = finishQueryBits qb bs2
        | otherwise
        = search qb

    -- Parse (',' \s* '?' \s*)* ')' [^?]* .
    --
    -- Since we've already consumed at least one '?', there's no turning back.
    -- The parse has to succeed here, or the whole thing fails
    -- (because we don't allow '?' to appear outside of the VALUES list).
    finishQueryBits qb bs0
        | Just (')', bs1) <- B.uncons bs0
        = if '?' `B.elem` bs1
              then Nothing
              else Just $ slice3 template qb bs1
        | Just (',', skipSpace -> bs1) <- B.uncons bs0
        , Just ('?', skipSpace -> bs2) <- B.uncons bs1
        = finishQueryBits qb bs2
        | otherwise
        = Nothing

    -- Slice a string into three pieces, given the start offset of the second
    -- and third pieces.  Each "offset" is actually a tail of the uppercase
    -- version of the template string.  Its length is used to infer the offset.
    --
    -- It is important to note that we only slice the original template.
    -- We don't want our all-caps trick messing up the actual query string.
    slice3 source p1 p2 =
        (s1, s2, source'')
      where
        (s1, source')  = B.splitAt (B.length source - B.length p1) source
        (s2, source'') = B.splitAt (B.length p1     - B.length p2) source'

    toUpper_ascii c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
                    | otherwise            = c

    -- Based on the definition of {ident_cont} in src/backend/parser/scan.l
    -- in the PostgreSQL source.  No need to check [a-z], since we converted
    -- the whole string to uppercase.
    isIdent c = (c >= '0'    && c <= '9')
             || (c >= 'A'    && c <= 'Z')
             || (c >= '\x80' && c <= '\xFF')
             || c == '_'
             || c == '$'

    -- Based on {space} in scan.l
    isSpace_ascii c = (c == ' ') || (c >= '\t' && c <= '\r')

    skipSpace = B.dropWhile isSpace_ascii

--escapeByteaConn :: Connection -> ByteString -> IO (Either ByteString ByteString)
--escapeByteaConn conn s =
--    withConnection conn $ \c ->
--    PQ.escapeByteaConn c s >>= checkError c

-- Escape string for sqlite
--
-- TODO It would be REALLY good to replace the use of this function by
-- either sqlite3_mprintf '%q' or better yet, using SQLite3's
-- parameter binding instead of our own substitution.  Rolling your
-- own escaping is prone to bugs that leave SQL injection holes.
escapeStringSql :: ByteString -> ByteString
escapeStringSql s = B.concatMap (\c -> if c == '\'' then "''" else B.singleton c) s

buildQuery :: Connection -> Query -> ByteString -> [Action] -> IO Builder
buildQuery conn q template xs = zipParams (split template) <$> mapM sub xs
  where quote = either (\msg -> fmtError (utf8ToString msg) q xs)
                       (inQuotes . fromByteString)
        utf8ToString = T.unpack . TE.decodeUtf8
        sub (Plain  b)      = pure b
        sub (Escape s)      = quote <$> return (Right (escapeStringSql s))
        sub (EscapeByteA s) = error "NOT IMPLEMENTED" ----quote <$> escapeByteaConn conn s
        sub (Many  ys)      = mconcat <$> mapM sub ys
        split s = fromByteString h : if B.null t then [] else split (B.tail t)
            where (h,t) = B.break (=='?') s
        zipParams (t:ts) (p:ps) = t `mappend` p `mappend` zipParams ts ps
        zipParams [t] []        = t
        zipParams _ _ = fmtError (show (B.count '?' template) ++
                                  " '?' characters, but " ++
                                  show (length xs) ++ " parameters") q xs


-- | Open a database connection to a given file.  Will throw an
-- exception if it cannot connect.
--
-- Every 'open' must be closed with a call to 'close'.
open :: String -> IO Connection
open fname = Connection <$> Base.open fname

-- | Close a database connection.
close :: Connection -> IO ()
close (Connection c) = Base.close c

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Query -> q -> IO ()
execute conn template qs = do
  formatQuery conn template qs >>= \q' -> execute_ conn (Query q')

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO ()
executeMany conn q qs = do
  formatMany conn q qs >>= \q' -> execute_ conn (Query q')

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
query :: (ToRow q, FromRow r)
         => Connection -> Query -> q -> IO [r]
query conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishQuery conn template result

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn q@(Query que) = do
  result <- exec conn que
  finishQuery conn q result

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO ()
execute_ (Connection conn) (Query que) =
  bracket (Base.prepare conn (utf8ToString que)) Base.finalize go
    where
      go stmt = void $ Base.step stmt


finishQuery :: (FromRow r) => Connection -> Query -> Result -> IO [r]
finishQuery conn q rows =
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

fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map twiddle xs
                    }
  where twiddle (Plain b)       = toByteString b
        twiddle (Escape s)      = s
        twiddle (EscapeByteA s) = s
        twiddle (Many ys)       = B.concat (map twiddle ys)
