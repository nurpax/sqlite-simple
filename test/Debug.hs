{-# LANGUAGE OverloadedStrings #-}

module Debug (
    testDebugTracing) where

import Control.Concurrent
import Common

-- Simplest SELECT
testDebugTracing :: TestEnv -> Test
testDebugTracing TestEnv{..} = TestCase $ do
  chan <- newChan
  let logger m = writeChan chan m
  setTrace conn (Just logger)
  execute_ conn "SELECT null"
  msg <- readChan chan
  "SELECT null" @=? msg
  execute conn "SELECT 1+?" (Only (2 :: Int))
  execute conn "SELECT 1+?" (Only (3 :: Int))
  msg <- readChan chan
  "SELECT 1+2" @=? msg
  msg <- readChan chan
  "SELECT 1+3" @=? msg
  -- Check that disabling works too
  setTrace conn Nothing
  execute_ conn "SELECT null"
  writeChan chan "empty"
  msg <- readChan chan
  "empty" @=? msg
