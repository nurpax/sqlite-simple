{-# LANGUAGE QuasiQuotes #-}

module QQ (
     testSimpleQQ
   , testMultiLinedQQ
   ) where

import Common
import Database.SQLite.Simple.QQ (sql)

testSimpleQQ :: TestEnv -> Test
testSimpleQQ TestEnv{..} = TestCase $ do
  q <- query_ conn "SELECT 1+1" :: IO [Only Int]
  qq <- query_ conn [sql|
                        SELECT 1 + 1
                        |] :: IO [Only Int]
  assertEqual "result" q qq


testMultiLinedQQ :: TestEnv -> Test
testMultiLinedQQ TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE testQQ (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO testQQ (t) VALUES ('test string')"
  q <- query_ conn "SELECT t FROM testQQ" :: IO [Only String]
  qq <- query_ conn [sql|
    SELECT
      t
    FROM
      testQQ

    |] :: IO [Only String]
  assertEqual "result" q qq
