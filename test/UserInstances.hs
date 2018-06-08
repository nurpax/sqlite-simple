{-# LANGUAGE DeriveDataTypeable #-}

module UserInstances (
   testUserFromField
  ,testSQLDataFromField
  ) where

import           Common
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import qualified Data.Text as T
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField

newtype MyType = MyType String deriving (Eq, Show, Typeable)

instance FromField MyType where
  fromField f = cvt f . fieldData $ f where
    -- Prefix with "fromField " to really ensure we got here
    cvt _ (SQLText s) = Ok $ MyType ("fromField "++(T.unpack s))
    cvt f _           = returnError ConversionFailed f "expecting SQLText type"

instance ToField MyType where
  -- Prefix with "toField " to really ensure we got here
  toField (MyType s) = SQLText . T.pack $ ("toField " ++ s)

testUserFromField :: TestEnv -> Test
testUserFromField TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE fromfield (t TEXT)"
  execute conn "INSERT INTO fromfield (t) VALUES (?)" (Only ("test string" :: String))
  [Only r] <- query_ conn "SELECT t FROM fromfield" :: IO [(Only MyType)]
  (MyType "fromField test string") @=? r
  execute_ conn "DELETE FROM fromfield"
  execute conn "INSERT INTO fromfield (t) VALUES (?)" (Only (MyType "test2"))
  [Only r] <- query_ conn "SELECT t FROM fromfield" :: IO [(Only String)]
  "toField test2" @=? r

testSQLDataFromField :: TestEnv -> Test
testSQLDataFromField TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE sqldatafromfield (t TEXT, i INT, b BOOLEAN, f FLOAT)"
  execute conn "INSERT INTO sqldatafromfield (t,i,b,f) VALUES (?,?,?,?)" (("test string" :: T.Text,
                                                                    1 :: Int64,
                                                                    True :: Bool,
                                                                    1.11 :: Double))
  execute conn "INSERT INTO sqldatafromfield (t,i,b) VALUES (?,?,?)" (("test string2" :: T.Text,
                                                                    2 :: Int64,
                                                                    False :: Bool))
  r <- query_ conn "SELECT * FROM sqldatafromfield" :: IO [[SQLData]]
  let testData = [[SQLText "test string",
                   SQLInteger 1,
                   SQLInteger 1,
                   SQLFloat 1.11],
                  [SQLText "test string2",
                   SQLInteger 2,
                   SQLInteger 0,
                   SQLNull]]
  testData @=? r
