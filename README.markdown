sqlite-simple: mid-level bindings to the sqlite database
========================================================

[![Build Status](https://secure.travis-ci.org/nurpax/sqlite-simple.png)](http://travis-ci.org/nurpax/sqlite-simple)

This library is a mid-level Haskell binding to the SQLite database.

Sqlite-simple provides a convenient API to sqlite that does some level
of automatic data conversion between the database and Haskell types.
The API has been modeled directly after
[postgresql-simple](http://github.com/lpsmith/postgresql-simple) which
in turn borrows from
[mysql-simple](https://github.com/bos/mysql-simple).

[The sqlite-simple API
reference](https://hackage.haskell.org/package/sqlite-simple/docs/Database-SQLite-Simple.html)
contains more examples of use and information on its features.

The library is well tested and stable.  The library should also be
reasonably performant.  You can find its benchmark suite here:
[db-bench](https://github.com/nurpax/db-bench).  You can read more
about sqlite-simple's expected performance in [my blog about
sqlite-simple performance against direct-sqlite, Python and
C](http://nurpax.github.io/posts/2013-08-17-sqlite-simple-benchmarking.html).


## Installation

You can install [sqlite-simple from Hackage](http://hackage.haskell.org/package/sqlite-simple)
with:

```
cabal install sqlite-simple
```

A Windows user?  It works but please see [this note](https://gist.github.com/3907344) on direct-sqlite Windows installation.

## Examples of use

Create a test database by copy&pasting the below snippet to your
shell:

```
sqlite3 test.db "CREATE TABLE test (id INTEGER PRIMARY KEY, str text);\
INSERT INTO test (str) VALUES ('test string');"
```

..and access it in Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
```

More simple usage examples can be found from [sqlite-simple unit
tests](https://github.com/nurpax/sqlite-simple/blob/master/test/Simple.hs).


## Development

The development roadmap for sqlite-simple is mostly captured in the
github issue database.

I'm happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/nurpax/sqlite-simple/issues).

For general database issues with a Haskell focus, I recommend sending
e-mail to the [database-devel mailing
list](http://www.haskell.org/mailman/listinfo/database-devel).

## Contributing

If you send pull requests for new features, it'd be great if you could also develop unit
tests for any such features.

## Credits

- [Janne Hellsten](https://github.com/nurpax) author, long-term maintainer
- [Sergey Bushnyak](https://github.com/sigrlami) current maintainer

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
