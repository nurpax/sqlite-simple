sqlite-simple: mid-level bindings to the sqlite database
========================================================

## THIS LIBRARY IS VERY MUCH WIP! (do not use yet)

This library is a mid-level Haskell binding to the SQLite database.

Examples of use
---------------

Create a test database by copy&pasting the below snippet to your
shell:

```
sqlite3 test.db "\
CREATE TABLE test (id INTEGER PRIMARY KEY, str text);\
INSERT INTO test (str) VALUES ('test string');"
```

And access it in Haskell:

```
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String
                 deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
```

Credits
-------

A lot of the code is directly borrowed from
[mysql-simple](http://github.com/bos/mysql-simple) by Bryan O'Sullivan
and from
[postgresql-simple](http://github.com/lpsmith/postgresql-simple) by
Leon P. Smith.

This package builds on top of the
[direct-sqlite](http://hackage.haskell.org/package/direct-sqlite)
package by Irene Knapp.

SQLite is rather weakly-typed and thus the SQL->Haskell type
strictness of the parent projects does not necessarily apply to this
package.
