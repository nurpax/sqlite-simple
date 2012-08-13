-- -*- coding: utf-8 -*-

module Utf8Strings (testUtf8Simplest) where

import Common

-- Simplest SELECT
testUtf8Simplest :: TestEnv -> Test
testUtf8Simplest TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE utf (id INTEGER, t TEXT)"
  execute_ conn "INSERT INTO utf (id, t) VALUES (1, 'ääöö')"
--  execute conn "INSERT INTO utf (id, t) VALUES (?,?)" (1 :: Int, "ääööåå" :: String)
  [Only t1] <- query conn "SELECT t FROM utf WHERE id = ?" (Only (1 :: Int))
  assertEqual "utf8" ("ääöö" :: String) t1
  assertBool "" True
