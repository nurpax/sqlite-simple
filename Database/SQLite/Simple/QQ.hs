{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.QQ
-- Copyright:   (c) 2011-2012 Leon P Smith
--              (c) 2018 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'sql' quasiquoter, for writing large @SQL@ statements.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.QQ
     ( sql
     ) where

import           Data.String                  (fromString)
import           Database.SQLite.Simple.Types (Query)
import           Language.Haskell.TH          (Exp, Q, appE, stringE)
import           Language.Haskell.TH.Quote    (QuasiQuoter (..))

{- | A quasiquoter for writing big @SQL@ queries.

One should consider turning on the @-XQuasiQuotes@ pragma in that module:

@
{-# LANGUAGE QuasiQuoter #-}

myQuery = query conn [sql|
    SELECT
      *
    FROM
      users
    WHERE jobTitle = ?
    |] jobTitle
@

-}
sql :: QuasiQuoter
sql = QuasiQuoter
    { quotePat  = error "Database.SQLite.Simple.QQ.sql: quasiquoter used in pattern context"
    , quoteType = error "Database.SQLite.Simple.QQ.sql: quasiquoter used in type context"
    , quoteDec  = error "Database.SQLite.Simple.QQ.sql: quasiquoter used in declaration context"
    , quoteExp  = sqlExp
    }

sqlExp :: String -> Q Exp
sqlExp = appE [| fromString :: String -> Query |] . stringE
