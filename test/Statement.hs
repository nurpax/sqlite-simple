{-# LANGUAGE OverloadedStrings #-}

module Statement (
    testBind
  , testDoubleBind
  , testPreparedStatements
  , testPreparedStatementsColumnCount
  ) where

import Common
import Data.Maybe(fromJust)

import qualified Database.SQLite3 as Base

testBind :: TestEnv -> Test
testBind TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_bind VALUES(1, 'result')"
  withStatement conn "SELECT t FROM test_bind WHERE id=?" $ \stmt ->
    withBind stmt [1::Int] $ do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "result" (Only "result") (fromJust row)

testDoubleBind :: TestEnv -> Test
testDoubleBind TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_double_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_double_bind VALUES(1, 'first result')"
  execute_ conn "INSERT INTO test_double_bind VALUES(2, 'second result')"
  withStatement conn "SELECT t FROM test_double_bind WHERE id=?" $ \stmt -> do
    withBind stmt [1::Int] $ do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "first result" (Only "first result") (fromJust row)

    withBind stmt [2::Int] $ do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "second result" (Only "second result") (fromJust row)

testPreparedStatements :: TestEnv -> Test
testPreparedStatements TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE ps (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO ps VALUES(1, 'first result')"
  execute_ conn "INSERT INTO ps VALUES(2, 'second result')"
  withStatement conn "SELECT t FROM ps WHERE id=?" $ \stmt -> do
    colName <- columnName stmt 0
    colName @?= "t"
    elems <- mapM (queryOne stmt) [1 :: Int, 2]
    ["first result" :: String, "second result"] @=? elems
    where
      queryOne stmt rowId =
        withBind stmt (Only rowId) $ do
          Just (Only r) <- nextRow stmt
          Nothing <- nextRow stmt :: IO (Maybe (Only String))
          return r

testPreparedStatementsColumnCount :: TestEnv -> Test
testPreparedStatementsColumnCount TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE ps2 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO ps2 VALUES(1, 'first result')"
  withStatement conn "SELECT t FROM ps2 WHERE id=?" $ \stmt -> do
    colName <- columnName stmt 0
    colName @?= "t"
    ColumnIndex colCount <- columnCount stmt
    colCount @?= 1
    let baseStatment = unStatement stmt
    colCountBase <- Base.columnCount baseStatment
    colCountBase @?= 1
