
module Errors (
    testErrorsColumns
  ) where

import Common

testErrorsColumns :: TestEnv -> Test
testErrorsColumns TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE cols (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO cols (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM cols" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  -- Mismatched number of output columns (selects two, dest type has 1 field)
  _rows <- query_ conn "SELECT id,t FROM cols" :: IO [Only Int]
  return ()
  