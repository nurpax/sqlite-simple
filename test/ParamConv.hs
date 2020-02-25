{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
#endif

module ParamConv (
    testParamConvNull
  , testParamConvInt
  , testParamConvIntWidths
  , testParamConvIntWidthsFromField
  , testParamConvFloat
  , testParamConvBools
  , testParamConvDateTime
  , testParamConvFromRow
  , testParamConvToRow
  , testParamConvComposite
  , testParamNamed) where

import           Data.Int
import           Data.Word
import           Data.Time
import qualified Data.Text as T
import           Database.SQLite.Simple.Types (Null(..))

import Common

one, two, three :: Int
one   = 1
two   = 2
three = 3

testParamConvNull :: TestEnv -> Test
testParamConvNull TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE nulltype (id INTEGER PRIMARY KEY, t1 TEXT)"
  [Only r] <- (query_ conn "SELECT NULL") :: IO [Only Null]
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (one, r)
  [Only mr1] <- query_ conn "SELECT t1 FROM nulltype WHERE id = 1" :: IO [Only (Maybe String)]
  assertEqual "nulls" Nothing mr1
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (two, "foo" :: String)
  [mr2] <- query_ conn "SELECT t1 FROM nulltype WHERE id = 2" :: IO [Only (Maybe String)]
  assertEqual "nulls" (Just "foo") (fromOnly mr2)

testParamConvInt :: TestEnv -> Test
testParamConvInt TestEnv{..} = TestCase $ do
  [Only r] <- (query conn "SELECT ?" (Only one)) :: IO [Only Int]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only one)) :: IO [Only Integer]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?+?" (one, two)) :: IO [Only Int]
  assertEqual "value" 3 r
  [Only r] <- (query conn "SELECT ?+?" (one, 15 :: Int64)) :: IO [Only Int]
  assertEqual "value" 16 r
  [Only r] <- (query conn "SELECT ?+?" (two, 14 :: Int32)) :: IO [Only Int]
  assertEqual "value" 16 r
  [Only r] <- (query conn "SELECT ?+?" (two, 14 :: Integer)) :: IO [Only Int]
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

testParamConvIntWidths :: TestEnv -> Test
testParamConvIntWidths TestEnv{..} = TestCase $ do
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Int8))) :: IO [Only Int]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int8))) :: IO [Only Int] -- wrap around
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int16))) :: IO [Only Int]
  assertEqual "value" 257 r
  [Only r] <- (query conn "SELECT ?" (Only (258 :: Int32))) :: IO [Only Int]
  assertEqual "value" 258 r
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Word8))) :: IO [Only Int]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Word8))) :: IO [Only Int] -- wrap around
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Word16))) :: IO [Only Int]
  assertEqual "value" 257 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Word32))) :: IO [Only Int]
  assertEqual "value" 257 r
  [Only r] <- (query conn "SELECT ?" (Only (0x100000000 :: Word64))) :: IO [Only Int]
  assertEqual "value" 0x100000000 r
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Integer))) :: IO [Only Int]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Word))) :: IO [Only Int]
  assertEqual "value" 1 r

testParamConvIntWidthsFromField :: TestEnv -> Test
testParamConvIntWidthsFromField TestEnv{..} = TestCase $ do
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Int))) :: IO [Only Int8]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int))) :: IO [Only Int8] -- wrap around
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (65536 :: Int))) :: IO [Only Int16] -- wrap around
  assertEqual "value" 0 r
  [Only r] <- (query conn "SELECT ?" (Only (65536 :: Int))) :: IO [Only Int32] -- wrap around
  assertEqual "value" 65536 r
  [Only r] <- (query conn "SELECT ?" (Only (258 :: Int))) :: IO [Only Int32]
  assertEqual "value" 258 r
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Int))) :: IO [Only Word8]
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int))) :: IO [Only Word8] -- wrap around
  assertEqual "value" 1 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int))) :: IO [Only Word16]
  assertEqual "value" 257 r
  [Only r] <- (query conn "SELECT ?" (Only (257 :: Int))) :: IO [Only Word32]
  assertEqual "value" 257 r
  [Only r] <- (query conn "SELECT ?" (Only (0x100000000 :: Int64))) :: IO [Only Word64]
  assertEqual "value" 0x100000000 r
  [Only r] <- (query conn "SELECT ?" (Only (1 :: Int))) :: IO [Only Word]
  assertEqual "value" 1 r

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
    (read "2012-08-12" :: Day, read "2012-08-12 01:01:01 UTC" :: UTCTime)
  [_,(t1,t2)] <- query_ conn "SELECT t1,t2 from dt" :: IO [(Day, UTCTime)]
  assertEqual "day" (read "2012-08-12" :: Day) t1
  assertEqual "day" (read "2012-08-12 01:01:01 UTC" :: UTCTime) t2


testParamConvBools :: TestEnv -> Test
testParamConvBools TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE bt (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- Booleans are ints with values 0 or 1 on SQLite
  execute_ conn "INSERT INTO bt (b) VALUES (0)"
  execute_ conn "INSERT INTO bt (b) VALUES (1)"
  [Only r1, Only r2] <- query_ conn "SELECT b from bt" :: IO [Only Bool]
  assertEqual "bool" False r1
  assertEqual "bool" True r2
  execute conn "INSERT INTO bt (b) VALUES (?)" (Only True)
  execute conn "INSERT INTO bt (b) VALUES (?)" (Only False)
  execute conn "INSERT INTO bt (b) VALUES (?)" (Only False)
  [Only r3, Only r4, Only r5] <-
    query_ conn "SELECT b from bt where id in (3, 4, 5) ORDER BY id" :: IO [Only Bool]
  assertEqual "bool" True r3
  assertEqual "bool" False r4
  assertEqual "bool" False r5

testParamConvFromRow :: TestEnv -> Test
testParamConvFromRow TestEnv{..} = TestCase $ do
  [(1,2)] <- query_ conn "SELECT 1,2" :: IO [(Int,Int)]
  [(1,2,3)] <- query_ conn "SELECT 1,2,3" :: IO [(Int,Int,Int)]
  [(1,2,3,4)] <- query_ conn "SELECT 1,2,3,4" :: IO [(Int,Int,Int,Int)]
  [(1,2,3,4,5)] <- query_ conn "SELECT 1,2,3,4,5" :: IO [(Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6)] <- query_ conn "SELECT 1,2,3,4,5,6" :: IO [(Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7)] <- query_ conn "SELECT 1,2,3,4,5,6,7" :: IO [(Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8,9)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8,9" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8,9,10)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8,9,10" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)]
  [[1,2,3]] <- query_ conn "SELECT 1,2,3" :: IO [[Int]]
  return ()

testParamConvToRow :: TestEnv -> Test
testParamConvToRow TestEnv{..} = TestCase $ do
  [Only (s :: Int)] <- query conn "SELECT 13" ()
  13 @=? s
  [Only (s :: Int)] <- query conn "SELECT ?" (Only one)
  1 @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?" (one, two)
  (1+2) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?" (one, two, three)
  (1+2+3) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?" (one, two, three, 4 :: Int)
  (1+2+3+4) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int)
  (1+2+3+4+5) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int)
  (1+2+3+4+5+6) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int)
  (1+2+3+4+5+6+7) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int)
  (1+2+3+4+5+6+7+8) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int)
  (1+2+3+4+5+6+7+8+9) @=? s
  [Only (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int, 10 :: Int)
  (1+2+3+4+5+6+7+8+9+10) @=? s

data TestTuple  = TestTuple  Int64 Int64 deriving (Eq, Show)
data TestTuple2 = TestTuple2 T.Text T.Text deriving (Eq, Show)

instance FromRow TestTuple where
  fromRow = TestTuple <$> field <*> field

instance FromRow TestTuple2 where
  fromRow = TestTuple2 <$> field <*> field

instance ToRow TestTuple where
  toRow (TestTuple a b) = [SQLInteger a, SQLInteger b]

instance ToRow TestTuple2 where
  toRow (TestTuple2 a b) = [SQLText a, SQLText b]

testParamConvComposite :: TestEnv -> Test
testParamConvComposite TestEnv{..} = TestCase $ do
  [t1] <- query_ conn "SELECT 1,2"
  TestTuple 1 2 @=? t1
  [t2] <- query_ conn "SELECT 'foo','bar'"
  TestTuple2 "foo" "bar" @=? t2
  [a :. b] <- query_ conn "SELECT 4,5,'baz','xyzz'"
  TestTuple 4 5 :. TestTuple2 "baz" "xyzz" @=? a :. b
  [TestTuple x y :. TestTuple2 z w] <- query conn "SELECT ?,?,?,?" (a :. b)
  x @=? 4
  y @=? 5
  z @=? "baz"
  w @=? "xyzz"

testParamNamed :: TestEnv -> Test
testParamNamed TestEnv{..} = TestCase $ do
  [Only t1] <- queryNamed conn "SELECT :foo / :bar" [":foo" := two, ":bar" := one]
  t1 @=? (2 :: Int)
  [(t1,t2)] <- queryNamed conn "SELECT :foo,:bar" [":foo" := ("foo" :: T.Text), ":bar" := one]
  t1 @=? ("foo" :: T.Text)
  t2 @=? one
  execute_ conn "CREATE TABLE np (id INTEGER PRIMARY KEY, b BOOLEAN)"
  executeNamed conn "INSERT INTO np (b) VALUES (:b)" [":b" := True]
  [Only t1] <- query_ conn "SELECT b FROM np"
  True @=? t1
