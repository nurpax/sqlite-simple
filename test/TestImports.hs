
module TestImports where

-- Test file to test that we can do most things with a single import
import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple

data Test = Test Int Int Int

-- Hook up sqlite-simple to know how to read Test rows
instance FromRow Test where
  fromRow = Test <$> field <*> field <*> field

foo :: IO ()
foo = do
  conn <- open ":memory:"
  [Only _v] <- query_ conn "SELECT * FROM test" :: IO [Only Int]
  [_v] <- query_ conn "SELECT * FROM test" :: IO [(Int,Int)]
  [_v] <- query_ conn "SELECT * FROM test" :: IO [Test]
  [_v] <- query conn "SELECT ?+?" (3::Int,4::Int):: IO [(Only Int)]
  close conn

foo2 :: IO ()
foo2 = do
  conn <- open ":memory:"
  [Only _v] <- query_ conn (Query q) :: IO [Only Int]
  close conn
  where
    q = T.concat ["SELECT * FROM ", "test"]
