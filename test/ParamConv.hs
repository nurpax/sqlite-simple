
module ParamConv (
    testParamConvInt
  , testParamConvFloat) where

import Common

testParamConvInt :: TestEnv -> Test
testParamConvInt TestEnv{..} = TestCase $ do
  rows <- query_ conn "SELECT 1+1" :: IO [Only Int]
  assertEqual "row count" 1 (length rows)
  assertEqual "value" (Only 2) (head rows)

testParamConvFloat :: TestEnv -> Test
testParamConvFloat TestEnv{..} = TestCase $ do
  assertBool "duh" True
