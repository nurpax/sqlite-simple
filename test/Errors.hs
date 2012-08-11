{-# LANGUAGE ScopedTypeVariables #-}

module Errors (
    testErrorsColumns
  ) where

import Prelude hiding (catch)
import Control.Exception
import Common

testErrorsColumns :: TestEnv -> Test
testErrorsColumns TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE cols (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO cols (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM cols" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  -- Mismatched number of output columns (selects two, dest type has 1 field)
  convFailedTriggered <-
    catch ((query_ conn "SELECT id,t FROM cols" :: IO [Only Int]) >> return False)
    (\(_ :: ResultError) -> return True)
  assertBool "exception" convFailedTriggered
  -- Same as above but the other way round (select 1, dst has two)
  convFailed <-
    catch ((query_ conn "SELECT id FROM cols" :: IO [(Int, String)]) >> return False)
    (\(_ :: ResultError) -> return True)
  assertBool "exception" convFailed
  return ()
