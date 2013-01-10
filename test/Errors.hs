{-# LANGUAGE ScopedTypeVariables #-}

module Errors (
    testErrorsColumns
  , testErrorsInvalidParams
  , testErrorsWithStatement
  ) where

import Prelude hiding (catch)
import Control.Exception
import qualified Data.ByteString as B
import Data.Word

import Common
import Database.SQLite3 (SQLError)

assertResultErrorCaught :: IO a -> Assertion
assertResultErrorCaught action = do
  catch (action >> return False) (\(_ :: ResultError) -> return True) >>=
    assertBool "assertResultError exc"

assertFormatErrorCaught :: IO a -> Assertion
assertFormatErrorCaught action = do
  catch (action >> return False) (\(_ :: FormatError) -> return True) >>=
    assertBool "assertFormatError exc"

assertSQLErrorCaught :: IO a -> Assertion
assertSQLErrorCaught action = do
  catch (action >> return False) (\(_ :: SQLError) -> return True) >>=
    assertBool "assertErrorError exc"

testErrorsColumns :: TestEnv -> Test
testErrorsColumns TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE cols (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO cols (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM cols" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  -- Mismatched number of output columns (selects two, dest type has 1 field)
  assertResultErrorCaught (query_ conn "SELECT id,t FROM cols" :: IO [Only Int])
  -- Same as above but the other way round (select 1, dst has two)
  assertResultErrorCaught (query_ conn "SELECT id FROM cols" :: IO [(Int, String)])
  -- Mismatching types (source int,text doesn't match dst string,int
  assertResultErrorCaught (query_ conn "SELECT id, t FROM cols" :: IO [(String, Int)])
  -- Trying to get a blob into a string
  let d = B.pack ([0..127] :: [Word8])
  execute_ conn "CREATE TABLE cols_blobs (id INTEGER, b BLOB)"
  execute conn "INSERT INTO cols_blobs (id, b) VALUES (?,?)" (1 :: Int, d)
  assertResultErrorCaught
    (do [Only _t1] <- query conn "SELECT b FROM cols_blobs WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
        return ())
  execute_ conn "CREATE TABLE cols_bools (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- 3 = invalid value for bool, must be 0 or 1
  execute_ conn "INSERT INTO cols_bools (b) VALUES (3)"
  assertResultErrorCaught
    (do [Only _t1] <- query_ conn "SELECT b FROM cols_bools" :: IO [Only Bool]
        return ())


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

testErrorsWithStatement :: TestEnv -> Test
testErrorsWithStatement TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invstat (id INTEGER PRIMARY KEY, t TEXT)"
  assertSQLErrorCaught $
    withStatement conn "SELECT id, t, t1 FROM invstat" $ \_stmt ->
      assertFailure "Error not detected"
