{-# LANGUAGE DeriveDataTypeable #-}

module UserInstances (testUserFromField) where

import           Common
import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Typeable (Typeable)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField

newtype MyType = MyType String deriving (Eq, Show, Typeable)

newtype CustomField = CustomField Double deriving (Eq, Show, Typeable)
data FooType = FooType String CustomField deriving (Eq, Show, Typeable)

customFromText :: T.Text -> Either String CustomField
customFromText = fmap (CustomField . fst) . T.rational

instance FromRow FooType where
  fromRow = FooType <$> field <*> fieldWithParser customFromText

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
  assertEqual "fromField" (MyType "fromField test string") r
  execute_ conn "DELETE FROM fromfield"
  execute conn "INSERT INTO fromfield (t) VALUES (?)" (Only (MyType "test2"))
  [Only r] <- query_ conn "SELECT t FROM fromfield" :: IO [(Only String)]
  assertEqual "tofield" "toField test2" r
  -- fieldWithParser
  execute_ conn "DELETE FROM fromfield"
  execute_ conn "INSERT INTO fromfield (t) VALUES (13.0)"
  [FooType str (CustomField dbl)] <- query_ conn "SELECT t,t FROM fromfield"
  assertEqual "tofield" 13.0 dbl
  assertEqual "tofield" "13.0" str
