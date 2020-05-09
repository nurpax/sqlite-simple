{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DirectSqlite (
    testDirectSqlite
  ) where

import Common

import Control.Exception (bracket)
import qualified Database.SQLite3 as DS

testDirectSqlite :: TestEnv -> Test
testDirectSqlite TestEnv{..} = TestCase $ do
  let dsConn = connectionHandle conn
  bracket (DS.prepare dsConn "SELECT 1+1") DS.finalize testDirect
  [Only (res :: Int)] <- query_ conn "SELECT 1+2"
  assertEqual "1+2" 3 res
  where
    testDirect stmt = do
      DS.Row <- DS.step stmt
      res <- DS.column stmt 0
      assertEqual "1+1" (SQLInteger 2) res
