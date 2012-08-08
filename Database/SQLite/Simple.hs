
module Database.SQLite.Simple (
    open
  , close
  ) where

import Control.Applicative
import Database.SQLite.Simple.Types
import qualified Database.SQLite3 as Base

--import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow

data Connection = Connection Base.Database

open :: String -> IO Connection
open fname = Connection <$> Base.open fname

close :: Connection -> IO ()
close (Connection c) = Base.close c


-- TODO ToRow needed too!!
query :: (FromRow r)
         => Connection -> Query -> IO [r]
query conn template =
  return []

--  result <- exec conn =<< formatQuery conn template qs
--  finishQuery conn template result
