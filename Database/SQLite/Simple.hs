
module Database.SQLite.Simple (
    open
  , close
  ) where

import Control.Applicative
import qualified Database.SQLite3 as Base

data Connection = Connection Base.Database

open :: String -> IO Connection
open fname = Connection <$> Base.open fname

close :: Connection -> IO ()
close (Connection c) = Base.close c
