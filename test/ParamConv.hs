
module ParamConv (
    testParamConvInt
  , testParamConvFloat) where

import Data.Int

import Common

one :: Int
one = 1

two :: Int
two = 2

testParamConvInt :: TestEnv -> Test
testParamConvInt TestEnv{..} = TestCase $ do
  [Only r] <- (query conn "SELECT ?" (Only one)) :: IO [Only Int]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?+?" (one, two)) :: IO [Only Int]
  assertEqual "value" 3 r
  [Only r] <- (query conn "SELECT ?+?" (one, 15 :: Int64)) :: IO [Only Int]
  assertEqual "value" 16 r
  [Only r] <- (query conn "SELECT ?+?" (two, 14 :: Int32)) :: IO [Only Int]
  assertEqual "value" 16 r
  -- This overflows 32-bit ints, verify that we get more than 32-bits out
  [Only r] <- (query conn "SELECT 255*?" (Only (0x7FFFFFFF :: Int32))) :: IO [Only Int64]
  assertEqual "> 32-bit result"
    (255*0x7FFFFFFF :: Int64) (fromIntegral r)
  [Only r] <- (query conn "SELECT 2*?" (Only (0x7FFFFFFFFF :: Int64))) :: IO [Only Int64]
  assertEqual "> 32-bit result & param"
    (2*0x7FFFFFFFFF :: Int64) (fromIntegral r)
  [Only r] <- (query_ conn "SELECT NULL") :: IO [Only (Maybe Int)]
  assertEqual "should see nothing" Nothing r
  [Only r] <- (query_ conn "SELECT 3") :: IO [Only (Maybe Int)]
  assertEqual "should see Just 3" (Just 3) r
  [Only r] <- (query conn "SELECT ?") (Only (Nothing :: Maybe Int)) :: IO [Only (Maybe Int)]
  assertEqual "should see nothing" Nothing r
  [Only r] <- (query conn "SELECT ?") (Only (Just 4 :: Maybe Int)) :: IO [Only (Maybe Int)]
  assertEqual "should see 4" (Just 4) r

testParamConvFloat :: TestEnv -> Test
testParamConvFloat TestEnv{..} = TestCase $ do
  [Only r] <- query conn "SELECT ?" (Only (1.0 :: Double)) :: IO [Only Double]
  assertEqual "value" 1.0 r
  [Only r] <- query conn "SELECT ?*0.25" (Only (8.0 :: Double)) :: IO [Only Double]
  assertEqual "value" 2.0 r
