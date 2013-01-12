{-# LANGUAGE OverloadedStrings #-}

module Statement (
    testBind
  , testDoubleBind
) where

import Common
import Data.Maybe(fromJust)

testBind :: TestEnv -> Test
testBind TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_bind VALUES(1, 'result')"
  withStatement conn "SELECT t FROM test_bind WHERE id=?" $ \stmt ->
    withBind stmt [1::Int] $ \stmt -> do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "result" (Only "result") (fromJust row)

testDoubleBind :: TestEnv -> Test
testDoubleBind TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_double_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_double_bind VALUES(1, 'first result')"
  execute_ conn "INSERT INTO test_double_bind VALUES(2, 'second result')"
  withStatement conn "SELECT t FROM test_double_bind WHERE id=?" $ \stmt -> do
    withBind stmt [1::Int] $ \stmt -> do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "first result" (Only "first result") (fromJust row)

    withBind stmt [2::Int] $ \stmt -> do
      row <- nextRow stmt :: IO (Maybe (Only String))
      assertEqual "second result" (Only "second result") (fromJust row)
