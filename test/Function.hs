{-# LANGUAGE OverloadedStrings #-}
module Function (
  testExternalAddition
  ) where

import Control.Monad
import Common
import Database.SQLite.Simple.Function

testExternalAddition :: TestEnv -> Test
testExternalAddition TestEnv{..} = TestCase $ do
  createFunction conn "my_add" ((+) :: Int -> Int -> Int)
  forM_ ([1 .. 10] :: [Int]) $ \x ->
    forM_ ([1 .. 10] :: [Int]) $ \y -> do
      [Only r] <- query conn "SELECT my_add(?,?)" (x,y)
      assertEqual (show x ++ " + " ++ show y ++ " = " ++ show r) (x + y) r
