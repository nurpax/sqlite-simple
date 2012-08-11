
module Common (
    module Database.SQLite.Simple
  , module Test.HUnit
  , TestEnv(..)
) where

import Test.HUnit

import Database.SQLite.Simple

data TestEnv
    = TestEnv
        { conn     :: Connection
            -- ^ Connection shared by all the tests
        , withConn :: forall a. (Connection -> IO a) -> IO a
            -- ^ Bracket for spawning additional connections
        }
