-- -*- coding: utf-8 -*-

module Utf8Strings (testUtf8Simplest
                  , testBlobs) where

import Common
import qualified Data.ByteString as B
import Data.Word

testUtf8Simplest :: TestEnv -> Test
testUtf8Simplest TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE utf (id INTEGER, t TEXT)"
  execute_ conn "INSERT INTO utf (id, t) VALUES (1, 'ääöö')"
  execute conn "INSERT INTO utf (id, t) VALUES (?,?)" (2 :: Int, "ääööåå" :: String)
  [Only t1] <- query conn "SELECT t FROM utf WHERE id = ?" (Only (1 :: Int))
  assertEqual "utf8" ("ääöö" :: String) t1
  [Only t2] <- query conn "SELECT t FROM utf WHERE id = ?" (Only (2 :: Int))
  assertEqual "utf8" ("ääööåå" :: String) t2

testBlobs :: TestEnv -> Test
testBlobs TestEnv{..} = TestCase $ do
  let d = B.pack ([0..127] :: [Word8])
  execute_ conn "CREATE TABLE blobs (id INTEGER, b BLOB)"
  execute conn "INSERT INTO blobs (id, b) VALUES (?,?)" (1 :: Int, d)
  [Only t1] <- query conn "SELECT b FROM blobs WHERE id = ?" (Only (1 :: Int))
  assertEqual "blob" d t1
  assertEqual "blob nul char" 0 (B.index d 0)
  assertEqual "blob first char" 1 (B.index d 1)
