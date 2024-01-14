{-# LANGUAGE ScopedTypeVariables #-}

module Errors (
    testErrorsColumns
  , testErrorsInvalidParams
  , testErrorsInvalidNamedParams
  , testErrorsWithStatement
  , testErrorsColumnName
  , testErrorsTransaction
  , testErrorsImmediateTransaction
  , testErrorsExclusiveTransaction
  , testErrorsSavepoint
  ) where

import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Word
import           Data.Time (Day, UTCTime)

import           Common
import           Database.SQLite.Simple.Types (Null)

assertResultErrorThrown :: IO a -> ResultError -> Assertion
assertResultErrorThrown action expectedError =
  catch
    (action >> assertFailure ("Expected error: " ++ show expectedError ++ ", but nothing was thrown"))
    (\(e :: ResultError) -> assertEqual "assertResultErrorThrown" expectedError e)

assertFormatErrorCaught :: IO a -> Assertion
assertFormatErrorCaught action = do
  catch (action >> return False) (\(e :: FormatError) -> length (show e) `seq` return True) >>=
    assertBool "assertFormatError exc"

assertSQLErrorCaught :: IO a -> Assertion
assertSQLErrorCaught action = do
  catch (action >> return False) (\(e :: SQLError) -> length (show e) `seq` return True) >>=
    assertBool "assertSQLError exc"

assertOOBCaught :: IO a -> Assertion
assertOOBCaught action = do
  catch (action >> return False) (\(e :: ArrayException) -> length (show e) `seq` return True) >>=
    assertBool "assertOOBCaught exc"

testErrorsColumns :: TestEnv -> Test
testErrorsColumns TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE cols (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO cols (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM cols" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  -- Mismatched number of output columns (selects two, dest type has 1 field)
  assertResultErrorThrown (query_ conn "SELECT id,t FROM cols" :: IO [Only Int])
    ConversionFailed
      { errSQLType = "2 values: [(\"INTEGER\",\"SQLInteger 1\"),(\"TEXT\",\"SQLText \\\"test s[...]\")]"
      , errHaskellType = "at least 1 slots in target type"
      , errMessage = "mismatch between number of columns to convert and number in target type"
      }
  -- Same as above but the other way round (select 1, dst has two)
  assertResultErrorThrown (query_ conn "SELECT id FROM cols" :: IO [(Int, String)])
    ConversionFailed
      { errSQLType = "1 values: [(\"INTEGER\",\"SQLInteger 1\")]"
      , errHaskellType = "at least 2 slots in target type"
      , errMessage = "mismatch between number of columns to convert and number in target type"
      }
  -- Mismatching types (source int,text doesn't match dst string,int)
  assertResultErrorThrown (query_ conn "SELECT id, t FROM cols" :: IO [(String, Int)])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "[Char]", errMessage = "expecting SQLText column type"}
  -- Mismatching types (source string doesn't match dst integer)
  assertResultErrorThrown (query_ conn "SELECT 'foo'" :: IO [Only Integer])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "Integer", errMessage = "need an int"}
  -- Mismatching types (sources don't match destination float/double type)
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only Double])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Double", errMessage = "expecting an SQLFloat column type"}
  assertResultErrorThrown (query_ conn "SELECT 'foo'" :: IO [Only Double])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "Double", errMessage = "expecting an SQLFloat column type"}
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only Float])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Float", errMessage = "expecting an SQLFloat column type"}
  assertResultErrorThrown (query_ conn "SELECT 'foo'" :: IO [Only Float])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "Float", errMessage = "expecting an SQLFloat column type"}
  -- Mismatching types (sources don't match destination bool type, or is out of bounds)
  assertResultErrorThrown (query_ conn "SELECT 'true'" :: IO [Only Bool])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "Bool", errMessage = "expecting an SQLInteger column type"}
  assertResultErrorThrown (query_ conn "SELECT 2" :: IO [Only Bool])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Bool", errMessage = "bool must be 0 or 1, got 2"}
  -- Mismatching types (sources don't match destination string types (text, string)
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only T.Text])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Text", errMessage = "need a text"}
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only LT.Text])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Text", errMessage = "need a text"}
  assertResultErrorThrown (query_ conn "SELECT 1.0" :: IO [Only T.Text])
    ConversionFailed {errSQLType = "FLOAT", errHaskellType = "Text", errMessage = "need a text"}
  assertResultErrorThrown (query_ conn "SELECT 1.0" :: IO [Only LT.Text])
    ConversionFailed {errSQLType = "FLOAT", errHaskellType = "Text", errMessage = "need a text"}
  -- Mismatching types (sources don't match destination string types (time/date)
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only UTCTime])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "UTCTime", errMessage = "expecting SQLText column type"}
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only Day])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Day", errMessage = "expecting SQLText column type"}
  -- Mismatching types (sources don't match destination bytestring)
  [Only (_ :: B.ByteString)] <-  query_ conn "SELECT X'3177'"
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only B.ByteString])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "ByteString", errMessage = "expecting SQLBlob column type"}
  assertResultErrorThrown (query_ conn "SELECT 1" :: IO [Only LB.ByteString])
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "ByteString", errMessage = "expecting SQLBlob column type"}
  assertResultErrorThrown (query_ conn "SELECT 'foo'" :: IO [Only B.ByteString])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "ByteString", errMessage = "expecting SQLBlob column type"}
  assertResultErrorThrown (query_ conn "SELECT 'foo'" :: IO [Only LB.ByteString])
    ConversionFailed {errSQLType = "TEXT", errHaskellType = "ByteString", errMessage = "expecting SQLBlob column type"}
  -- Trying to get a blob into a string
  let d = B.pack ([0..127] :: [Word8])
  execute_ conn "CREATE TABLE cols_blobs (id INTEGER, b BLOB)"
  execute conn "INSERT INTO cols_blobs (id, b) VALUES (?,?)" (1 :: Int, d)
  assertResultErrorThrown
    (do [Only _t1] <- query conn "SELECT b FROM cols_blobs WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
        return ())
    ConversionFailed {errSQLType = "BLOB", errHaskellType = "[Char]", errMessage = "expecting SQLText column type"}
  execute_ conn "CREATE TABLE cols_bools (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- 3 = invalid value for bool, must be 0 or 1
  execute_ conn "INSERT INTO cols_bools (b) VALUES (3)"
  assertResultErrorThrown
    (do [Only _t1] <- query_ conn "SELECT b FROM cols_bools" :: IO [Only Bool]
        return ())
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Bool", errMessage = "bool must be 0 or 1, got 3"}
  [Only (nullVal :: Null)] <- query_ conn "SELECT NULL"
  False @=? nullVal == nullVal
  False @=? nullVal /= nullVal
  assertResultErrorThrown
    (do [Only (_t1 :: Null)] <- query_ conn "SELECT 1" :: IO [Only Null]
        return ())
    ConversionFailed {errSQLType = "INTEGER", errHaskellType = "Null", errMessage = "data is not null"}

testErrorsInvalidParams :: TestEnv -> Test
testErrorsInvalidParams TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invparams (id INTEGER PRIMARY KEY, t TEXT)"
  -- Test that only unnamed params are accepted
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (t) VALUES (:v)" (Only ("foo" :: String)))
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (id, t) VALUES (:v,$1)" (3::Int, "foo" :: String))
  -- In this case, we have two bound params but only one given to
  -- execute.  This should cause an error.
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (id, t) VALUES (?, ?)" (Only (3::Int)))

testErrorsInvalidNamedParams :: TestEnv -> Test
testErrorsInvalidNamedParams TestEnv{..} = TestCase $ do
  -- Test that only unnamed params are accepted
  assertFormatErrorCaught
    (queryNamed conn "SELECT :foo" [":foox" := (1 :: Int)] :: IO [Only Int])
  -- In this case, we have two bound params but only one given to
  -- execute.  This should cause an error.
  assertFormatErrorCaught
    (queryNamed conn "SELECT :foo + :bar" [":foo" := (1 :: Int)] :: IO [Only Int])
  -- Can't use named params in SQL string with the unnamed query/exec variants
  assertFormatErrorCaught
    (query conn "SELECT :foo" (Only (1 :: Int)) :: IO [Only Int])

testErrorsWithStatement :: TestEnv -> Test
testErrorsWithStatement TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invstat (id INTEGER PRIMARY KEY, t TEXT)"
  assertSQLErrorCaught $
    withStatement conn "SELECT id, t, t1 FROM invstat" $ \_stmt ->
      assertFailure "Error not detected"

testErrorsColumnName :: TestEnv -> Test
testErrorsColumnName TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invcolumn (id INTEGER PRIMARY KEY, t TEXT)"
  assertOOBCaught $
    withStatement conn "SELECT id FROM invcolumn" $ \stmt ->
      columnName stmt (ColumnIndex (-1)) >> assertFailure "Error not detected"

testErrorsTransaction :: TestEnv -> Test
testErrorsTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE trans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withTransaction conn $ do
    executeNamed conn "INSERT INTO trans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [Only r] <- query_ conn "SELECT t FROM trans" :: IO [Only String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM trans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO trans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO trans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- query_ conn "SELECT t FROM trans" :: IO [Only String]
      case rows of
        [Only txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"

testErrorsImmediateTransaction :: TestEnv -> Test
testErrorsImmediateTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE itrans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withImmediateTransaction conn $ do
    executeNamed conn "INSERT INTO itrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [Only r] <- query_ conn "SELECT t FROM itrans" :: IO [Only String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM itrans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withImmediateTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO itrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO itrans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- query_ conn "SELECT t FROM itrans" :: IO [Only String]
      case rows of
        [Only txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"

testErrorsExclusiveTransaction :: TestEnv -> Test
testErrorsExclusiveTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE etrans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withExclusiveTransaction conn $ do
    executeNamed conn "INSERT INTO etrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [Only r] <- query_ conn "SELECT t FROM etrans" :: IO [Only String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM etrans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withExclusiveTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO etrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO etrans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- query_ conn "SELECT t FROM etrans" :: IO [Only String]
      case rows of
        [Only txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"

testErrorsSavepoint :: TestEnv -> Test
testErrorsSavepoint TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE strans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withSavepoint conn $ do
    executeNamed conn "INSERT INTO strans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [Only r] <- query_ conn "SELECT t FROM strans" :: IO [Only String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM strans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withSavepoint conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO strans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO strans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- query_ conn "SELECT t FROM strans" :: IO [Only String]
      case rows of
        [Only txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"
