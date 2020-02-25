{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Simple (
    testSimpleOnePlusOne
  , testSimpleSelect
  , testSimpleParams
  , testSimpleTime
  , testSimpleTimeFract
  , testSimpleInsertId
  , testSimpleMultiInsert
  , testSimpleUTCTime
  , testSimpleUTCTimeTZ
  , testSimpleUTCTimeParams
  , testSimpleQueryCov
  , testSimpleStrings
  , testSimpleChanges
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
-- orphan IsString instance in older byteString
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime, Day)

import           Common

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- Simplest SELECT
testSimpleOnePlusOne :: TestEnv -> Test
testSimpleOnePlusOne TestEnv{..} = TestCase $ do
  rows <- query_ conn "SELECT 1+1" :: IO [Only Int]
  assertEqual "row count" 1 (length rows)
  assertEqual "value" (Only 2) (head rows)

testSimpleSelect :: TestEnv -> Test
testSimpleSelect TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test1 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM test1" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "int,string" (1, "test string") (head rows)
  -- Add another row
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string 2')"
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "row count" 2 (length rows)
  assertEqual "int,string" (1, "test string") (rows !! 0)
  assertEqual "int,string" (2, "test string 2") (rows !! 1)
  [Only r] <- query_ conn "SELECT NULL" :: IO [Only (Maybe Int)]
  assertEqual "nulls" Nothing r
  [Only r] <- query_ conn "SELECT 1" :: IO [Only (Maybe Int)]
  assertEqual "nulls" (Just 1) r
  [Only r] <- query_ conn "SELECT 1.0" :: IO [Only Double]
  assertEqual "doubles" 1.0 r
  [Only r] <- query_ conn "SELECT 1.0" :: IO [Only Float]
  assertEqual "floats" 1.0 r

testSimpleParams :: TestEnv -> Test
testSimpleParams TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE testparams (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "CREATE TABLE testparams2 (id INTEGER, t TEXT, t2 TEXT)"
  [Only i] <- query conn "SELECT ?" (Only (42 :: Int))  :: IO [Only Int]
  assertEqual "select int param" 42 i
  execute conn "INSERT INTO testparams (t) VALUES (?)" (Only ("test string" :: String))
  rows <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  execute_ conn "INSERT INTO testparams (t) VALUES ('test2')"
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
  assertEqual "select params" "test string" row
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (2 :: Int)) :: IO [Only String]
  assertEqual "select params" "test2" row
  [Only r1, Only r2] <- query conn "SELECT t FROM testparams WHERE (id = ? OR id = ?)" (1 :: Int, 2 :: Int) :: IO [Only String]
  assertEqual "select params" "test string" r1
  assertEqual "select params" "test2" r2
  [Only i] <- query conn "SELECT ?+?" [42 :: Int, 1 :: Int] :: IO [Only Int]
  assertEqual "select int param" 43 i
  [Only d] <- query conn "SELECT ?" [2.0 :: Double] :: IO [Only Double]
  assertEqual "select double param" 2.0 d
  [Only f] <- query conn "SELECT ?" [4.0 :: Float] :: IO [Only Float]
  assertEqual "select double param" 4.0 f

testSimpleTime :: TestEnv -> Test
testSimpleTime TestEnv{..} = TestCase $ do
  let timestr = "2012-08-20 20:19:58"
      time    = read (timestr ++ " UTC") :: UTCTime
  execute_ conn "CREATE TABLE time (t TIMESTAMP)"
  execute conn "INSERT INTO time (t) VALUES (?)" (Only time)
  [Only t] <- query_ conn "SELECT * FROM time" :: IO [Only UTCTime]
  assertEqual "UTCTime conv" time t
  [Only t] <- query conn "SELECT * FROM time WHERE t = ?" (Only time) :: IO [Only UTCTime]
  assertEqual "UTCTime conv2" time t
  -- Try inserting timestamp directly as a string
  execute_ conn "CREATE TABLE time2 (t TIMESTAMP)"
  execute_ conn (Query (T.concat ["INSERT INTO time2 (t) VALUES ('", T.pack timestr, "')"]))
  [Only t] <- query_ conn "SELECT * FROM time2" :: IO [Only UTCTime]
  assertEqual "UTCTime" time t
  rows <- query conn "SELECT * FROM time2 WHERE t = ?" (Only time) :: IO [Only UTCTime]
  assertEqual "should see one row result" 1 (length rows)
  assertEqual "UTCTime" time t
  -- Days
  let daystr = "2013-08-21"
      day    = read daystr :: Day
  [Only day'] <- query_ conn (Query (T.concat ["SELECT '", T.pack daystr, "'"]))
  day @?= day'
  [Only day''] <- query conn "SELECT ?" (Only day)
  day @?= day''
  -- database timestamp -> day conversion is treated as an error, but
  -- try converting a timestamp to a date and see we get it back ok
  [Only dayx] <- query_ conn "SELECT date('2013-08-21 08:00:03.256887')"
  day @?= dayx

testSimpleTimeFract :: TestEnv -> Test
testSimpleTimeFract TestEnv{..} = TestCase $ do
  let timestr = "2012-08-17 08:00:03.256887"
      time    = read (timestr ++ " UTC") :: UTCTime
  -- Try inserting timestamp directly as a string
  execute_ conn "CREATE TABLE timefract (t TIMESTAMP)"
  execute_ conn (Query (T.concat ["INSERT INTO timefract (t) VALUES ('", T.pack timestr, "')"]))
  [Only t] <- query_ conn "SELECT * FROM timefract" :: IO [Only UTCTime]
  assertEqual "UTCTime" time t
  rows <- query conn "SELECT * FROM timefract WHERE t = ?" (Only time) :: IO [Only UTCTime]
  assertEqual "should see one row result" 1 (length rows)

testSimpleInsertId :: TestEnv -> Test
testSimpleInsertId TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_row_id (id INTEGER PRIMARY KEY, t TEXT)"
  execute conn "INSERT INTO test_row_id (t) VALUES (?)" (Only ("test string" :: String))
  id1 <- lastInsertRowId conn
  execute_ conn "INSERT INTO test_row_id (t) VALUES ('test2')"
  id2 <- lastInsertRowId conn
  1 @=? id1
  2 @=? id2
  rows <- query conn "SELECT t FROM test_row_id WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
  1 @=?  (length rows)
  (Only "test string") @=? (head rows)
  [Only row] <- query conn "SELECT t FROM test_row_id WHERE id = ?" (Only (2 :: Int)) :: IO [Only String]
  "test2" @=? row

testSimpleMultiInsert :: TestEnv -> Test
testSimpleMultiInsert TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE test_multi_insert (id INTEGER PRIMARY KEY, t1 TEXT, t2 TEXT)"
  executeMany conn "INSERT INTO test_multi_insert (t1, t2) VALUES (?, ?)" ([("foo", "bar"), ("baz", "bat")] :: [(String, String)])
  id2 <- lastInsertRowId conn
  2 @=? id2

  rows <- query_ conn "SELECT id,t1,t2 FROM test_multi_insert" :: IO [(Int, String, String)]
  [(1, "foo", "bar"), (2, "baz", "bat")] @=? rows

testSimpleUTCTime :: TestEnv -> Test
testSimpleUTCTime TestEnv{..} = TestCase $ do
  -- Time formats understood by sqlite: http://sqlite.org/lang_datefunc.html
  let timestrs = [ "2012-08-17 13:25"
                 , "2012-08-17 13:25:44"
                 , "2012-08-17 13:25:44.123"
                 ]
      timestrsWithT = map (T.map (\c -> if c == ' ' then 'T' else c)) timestrs
  execute_ conn "CREATE TABLE utctimes (t TIMESTAMP)"
  mapM_ (\t -> execute conn "INSERT INTO utctimes (t) VALUES (?)" (Only t)) timestrs
  mapM_ (\t -> execute conn "INSERT INTO utctimes (t) VALUES (?)" (Only t)) timestrsWithT
  dates <- query_ conn "SELECT t from utctimes" :: IO [Only UTCTime]
  mapM_ matchDates (zip (timestrs ++ timestrsWithT) dates)
  let zulu = "2012-08-17 13:25"
  [d] <- query conn "SELECT ?" (Only (T.append zulu "Z"))
  matchDates (zulu, d)
  let zulu = "2012-08-17 13:25:00"
  [d] <- query conn "SELECT ?" (Only (T.append zulu "Z"))
  matchDates (zulu, d)
  where
    matchDates (str,(Only date)) = do
      -- Remove 'T' when reading in to Haskell
      let t = read (makeReadable str) :: UTCTime
      t @=? date

    makeReadable s =
      let s' = if T.length s < T.length "YYYY-MM-DD HH:MM:SS" then T.append s ":00" else s
      in (T.unpack . T.replace "T" " " $ s') ++ " UTC"

testSimpleUTCTimeTZ :: TestEnv -> Test
testSimpleUTCTimeTZ TestEnv{..} = TestCase $ do
  -- Time formats understood by sqlite: http://sqlite.org/lang_datefunc.html
  let timestrs = [ "2013-02-03 13:00:00-02:00"
                 , "2013-02-03 13:00:00-01:00"
                 , "2013-02-03 13:00:00-03:00"
                 , "2013-02-03 13:00:00Z"
                 , "2013-02-03 13:00:00+00:00"
                 , "2013-02-03 13:00:00+03:00"
                 , "2013-02-03 13:00:00+02:00"
                 , "2013-02-03 13:00:00+04:00"
                 ]
  execute_ conn "CREATE TABLE utctimestz (t TIMESTAMP)"
  mapM_ (\t -> execute conn "INSERT INTO utctimestz (t) VALUES (?)" (Only t)) timestrs
  dates <- query_ conn "SELECT t from utctimestz" :: IO [Only UTCTime]
  mapM_ matchDates (zip (timestrs) dates)
  where
    matchDates (str,(Only date)) = do
      -- Remove 'T' when reading in to Haskell
      let t = read . T.unpack $ str :: UTCTime
      t @=? date

testSimpleUTCTimeParams :: TestEnv -> Test
testSimpleUTCTimeParams TestEnv{..} = TestCase $ do
  let times = [ "2012-08-17 08:00:03"
              , "2012-08-17 08:00:03.2"
              , "2012-08-17 08:00:03.256"
              , "2012-08-17 08:00:03.4192"
              ]
  -- Try inserting timestamp directly as a string
  mapM_ assertResult times
  where
    assertResult tstr = do
      let utct = read . (++ " UTC") . T.unpack $ tstr :: UTCTime
      [Only t] <- query conn "SELECT ?" (Only utct) :: IO [Only T.Text]
      assertEqual "UTCTime" tstr t

testSimpleQueryCov :: TestEnv -> Test
testSimpleQueryCov TestEnv{..} = TestCase $ do
  let str = "SELECT 1+1" :: T.Text
      q   = "SELECT 1+1" :: Query
  fromQuery q @=? str
  show str @=? show q
  q @=? ((read . show $ q) :: Query)
  q @=? q
  q @=? (Query "SELECT 1" <> Query "+1")
  q @=? foldr mappend mempty ["SELECT ", "1", "+", "1"]
  True @=? q <= q

testSimpleStrings :: TestEnv -> Test
testSimpleStrings TestEnv{..} = TestCase $ do
  [Only s] <- query_ conn "SELECT 'str1'"  :: IO [Only T.Text]
  s @=? "str1"
  [Only s] <- query_ conn "SELECT 'strLazy'"  :: IO [Only LT.Text]
  s @=? "strLazy"
  [Only s] <- query conn "SELECT ?" (Only ("strP" :: T.Text)) :: IO [Only T.Text]
  s @=? "strP"
  [Only s] <- query conn "SELECT ?" (Only ("strPLazy" :: LT.Text)) :: IO [Only T.Text]
  s @=? "strPLazy"
  -- ByteStrings are blobs in sqlite storage, so use ByteString for
  -- both input and output
  [Only s] <- query conn "SELECT ?" (Only ("strBsP" :: BS.ByteString)) :: IO [Only BS.ByteString]
  s @=? "strBsP"
  [Only s] <- query conn "SELECT ?" (Only ("strBsPLazy" :: LBS.ByteString)) :: IO [Only BS.ByteString]
  s @=? "strBsPLazy"
  [Only s] <- query conn "SELECT ?" (Only ("strBsPLazy2" :: BS.ByteString)) :: IO [Only LBS.ByteString]
  s @=? "strBsPLazy2"

testSimpleChanges :: TestEnv -> Test
testSimpleChanges TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE testchanges (id INTEGER PRIMARY KEY, t TEXT)"
  execute conn "INSERT INTO testchanges(t) VALUES (?)" (Only ("test string" :: String))
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute conn "INSERT INTO testchanges(t) VALUES (?)" (Only ("test string 2" :: String))
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo' WHERE id = 1"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo' WHERE id = 100"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 0 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo'"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 2 numChanges
