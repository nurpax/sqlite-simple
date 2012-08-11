
module Common (
    module Database.SQLite.Simple
  , module Test.HUnit
  , TestEnv(..)
) where

import Database.SQLite.Simple
import Test.HUnit

data TestEnv
    = TestEnv
        { conn     :: Connection
            -- ^ Connection shared by all the tests
        , withConn :: forall a. (Connection -> IO a) -> IO a
            -- ^ Bracket for spawning additional connections
        }
