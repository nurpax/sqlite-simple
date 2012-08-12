
module ParamConv (
    testParamConvInt
  , testParamConvFloat
  , testParamConvDateTime) where

import Data.Int
import Data.Time

import Common

one, two, three :: Int
one   = 1
two   = 2
three = 3

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
  [Only r] <- (query conn "SELECT ?") (Only (Just three :: Maybe Int)) :: IO [Only (Maybe Int)]
  assertEqual "should see 4" (Just 3) r

testParamConvFloat :: TestEnv -> Test
testParamConvFloat TestEnv{..} = TestCase $ do
  [Only r] <- query conn "SELECT ?" (Only (1.0 :: Double)) :: IO [Only Double]
  assertEqual "value" 1.0 r
  [Only r] <- query conn "SELECT ?*0.25" (Only (8.0 :: Double)) :: IO [Only Double]
  assertEqual "value" 2.0 r

testParamConvDateTime :: TestEnv -> Test
testParamConvDateTime TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE dt (id INTEGER PRIMARY KEY, t1 DATE, t2 TIMESTAMP)"
  execute_ conn "INSERT INTO dt (t1, t2) VALUES (date('now'), datetime('now'))"
  _rows <- query_ conn "SELECT t1,t2 from dt" :: IO [(Day, UTCTime)]
  -- TODO should _rows be forced to make sure parsers kick on the
  -- returned data?
  execute conn "INSERT INTO dt (t1,t2) VALUES (?,?)"
    (read "2012-08-12" :: Day, read "2012-08-12 01:01:01" :: UTCTime)
  [_,(t1,t2)] <- query_ conn "SELECT t1,t2 from dt" :: IO [(Day, UTCTime)]
  assertEqual "day" (read "2012-08-12" :: Day) t1
  assertEqual "day" (read "2012-08-12 01:01:01" :: UTCTime) t2

