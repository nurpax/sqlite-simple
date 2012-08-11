
module Simple (
    testSimpleOnePlusOne
  , testSimpleSelect
  , testSimpleParams) where

import Common

-- Simplest SELECT
testSimpleOnePlusOne :: TestEnv -> Test
testSimpleOnePlusOne TestEnv{..} = TestCase $ do
  rows <- query_ conn "SELECT 1+1" :: IO [Only Int]
  assertEqual "row count" 1 (length rows)
  assertEqual "value" (Only 2) (head rows)

testSimpleSelect :: TestEnv -> Test
testSimpleSelect TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test1 (id INTEGER PRIMARY KEY, t TEXT)"
  -- TODO another case needed for inserting with real query params
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM test1" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "int,string" (1, "test string") (head rows)
  -- Add another row
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string 2')"
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "row count" 2 (length rows)
  assertEqual "int,string" (1, "test string") (rows !! 0)
  assertEqual "int,string" (2, "test string 2") (rows !! 1)

testSimpleParams :: TestEnv -> Test
testSimpleParams TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE testparams (id INTEGER PRIMARY KEY, t TEXT)"
  -- TODO another case needed for inserting with real query params
  [Only i] <- (query conn "SELECT ?" [42 :: Int])  :: IO [Only Int]
  assertEqual "select int param" 42 i
  execute_ conn "INSERT INTO testparams (t) VALUES ('test string')"
  rows <- query conn "SELECT t FROM test1 WHERE id = ?" [(1 :: Int)] :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  execute_ conn "INSERT INTO testparams (t) VALUES ('test2')"
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" [(1 :: Int)] :: IO [Only String]
  assertEqual "select params" "test string" row
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" [(2 :: Int)] :: IO [Only String]
  assertEqual "select params" "test2" row
  return ()
