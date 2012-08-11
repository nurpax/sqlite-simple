{-# LANGUAGE ScopedTypeVariables #-}

module Errors (
    testErrorsColumns
  , testErrorsInvalidParams
  ) where

import Prelude hiding (catch)
import Control.Exception
import Common

assertResultErrorCaught :: IO a -> Assertion
assertResultErrorCaught action = do
  catch (action >> return False) (\(_ :: ResultError) -> return True) >>=
    assertBool "assertResultError exc"

assertFormatErrorCaught :: IO a -> Assertion
assertFormatErrorCaught action = do
  catch (action >> return False) (\(_ :: FormatError) -> return True) >>=
    assertBool "assertResultError exc"

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

testErrorsInvalidParams :: TestEnv -> Test
testErrorsInvalidParams TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invparams (id INTEGER PRIMARY KEY, t TEXT)"
  -- No parameters bound.  SQLite will silently subst NULL to unbound
  -- variables.  Call to execute_ will succeed, although it's probably
  -- not what users of this library would want.
  execute_ conn "INSERT INTO invparams (t) VALUES (:v)"
  -- In this case, we have two bound params but only one given to
  -- execute.  This should cause an error.
  assertFormatErrorCaught (execute conn "INSERT INTO invparams (id, t) VALUES (?, ?)" [3 :: Int])
  -- TODO we should actually disallow ?1,?2, :v etc since our input
  -- query params don't specify any binding to names/indices.
  [Only row] <- query_ conn "SELECT t FROM invparams" :: IO [Only (Maybe String)]
  assertEqual "string" Nothing row
