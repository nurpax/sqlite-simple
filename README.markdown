# sqlite-simple: mid-level bindings to the sqlite database

This library is a mid-level Haskell binding to the SQLite database.

Borrows almost directly from
[mysql-simple](http://github.com/bos/mysql-simple) and
[postgresql-simple](http://github.com/lpsmith/postgresql-simple).  

SQLite is rather weakly-typed and thus the SQL->Haskell type
strictness of the parent projects does not necessarily apply to this
package.
