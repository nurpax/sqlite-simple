------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.Time
-- Copyright:   (c) 2012 Leon P Smith
--              (c) 2012-2014 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
--
-- Conversions to/from Haskell 'UTCTime' and 'Day' types for SQLite3.
-- Offers better performance than direct use of time package's
-- 'read'/'show' functionality.
--
-- The parsers are heavily adapted for the specific variant of ISO 8601 that
-- SQLite uses,  and the printers attempt to duplicate this syntax.
------------------------------------------------------------------------------

module Database.SQLite.Simple.Time (
    module Database.SQLite.Simple.Time.Implementation
  ) where

import Database.SQLite.Simple.Time.Implementation
