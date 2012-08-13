sqlite-simple: mid-level bindings to the sqlite database
========================================================

## THIS LIBRARY IS VERY MUCH WIP! Use at your own risk

This library is a mid-level Haskell binding to the SQLite database.

Sqlite-simple provides a convenient API to sqlite that does some level
of automatic data conversion between the database and Haskell types.
The API has been modeled directly after
[postgresql-simple](http://github.com/lpsmith/postgresql-simple) which
in turn borrows from
[mysql-simple](https://github.com/bos/mysql-simple).

Building
--------

The usual cabal/cabal-dev instructions apply.

[![Continuous Integration status][status-png]][status]

NOTE: as of Aug 12, the package relies on an unreleased version of
direct-sqlite, so the git HEAD currently doesn't build out of the box.

Examples of use
---------------

Create a test database by copy&pasting the below snippet to your
shell:

```
sqlite3 test.db "CREATE TABLE test (id INTEGER PRIMARY KEY, str text);\
INSERT INTO test (str) VALUES ('test string');"
```

..and access it in Haskell:

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

More simple usage examples can be found from [sqlite-simple unit
tests](https://github.com/nurpax/sqlite-simple/blob/master/test/Simple.hs).


Development
-----------

The development roadmap for sqlite-simple is mostly captured in the
github issue database.

I'm happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/nurpax/sqlite-simple/issues).

For general database issues with a Haskell focus, I recommend sending
e-mail to the [database-devel mailing
list](http://www.haskell.org/mailman/listinfo/database-devel).


Credits
-------

A lot of the code is directly borrowed from
[mysql-simple](http://github.com/bos/mysql-simple) by Bryan O'Sullivan
and from
[postgresql-simple](http://github.com/lpsmith/postgresql-simple) by
Leon P. Smith.  Like Leon in postgresql-simple, I borrow code and
documentation directly from both of these ancestor libraries.

This package builds on top of the
[direct-sqlite](http://hackage.haskell.org/package/direct-sqlite)
package by Irene Knapp.

SQLite is rather weakly-typed and thus the SQL to Haskell type
strictness of the parent projects does not necessarily apply to this
package.
