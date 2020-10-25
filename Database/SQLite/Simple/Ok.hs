{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}

------------------------------------------------------------------------------
-- |
-- Module:     Database.SQLite.Simple.Ok
-- Copyright:  (c) 2012 Leon P Smith
--             (c) 2012-2013 Janne Hellsten
-- License:    BSD3
-- Maintainer: Janne Hellsten <jjhellst@gmail.com>
--
-- The 'Ok' type is a simple error handler,  basically equivalent to
-- @Either [SomeException]@.
--
-- One of the primary reasons why this type  was introduced is that
-- @Either SomeException@ had not been provided an instance for 'Alternative',
-- and it would have been a bad idea to provide an orphaned instance for a
-- commonly-used type and typeclass included in @base@.
--
-- Extending the failure case to a list of 'SomeException's enables a
-- more sensible 'Alternative' instance definitions:   '<|>' concatinates
-- the list of exceptions when both cases fail,  and 'empty' is defined as
-- 'Errors []'.   Though '<|>' one could pick one of two exceptions, and
-- throw away the other,  and have 'empty' provide a generic exception,
-- this avoids cases where 'empty' overrides a more informative exception
-- and allows you to see all the different ways your computation has failed.
--
------------------------------------------------------------------------------

module Database.SQLite.Simple.Ok where

import Control.Applicative
import Control.Exception
import Control.Monad (MonadPlus(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable

#if !MIN_VERSION_base(4,13,0) && MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

-- FIXME:   [SomeException] should probably be something else,  maybe
--          a difference list (or a tree?)

data Ok a = Errors [SomeException] | Ok !a
    deriving(Show, Typeable, Functor)

-- | Two 'Errors' cases are considered equal, regardless of what the
--   list of exceptions looks like.

instance Eq a => Eq (Ok a) where
    Errors _ == Errors _  = True
    Ok  a    == Ok  b     = a == b
    _        == _         = False

instance Applicative Ok where
    pure = Ok

    Errors es <*> _ = Errors es
    _ <*> Errors es = Errors es
    Ok f <*> Ok a   = Ok (f a)

instance Alternative Ok where
    empty = Errors []

    a@(Ok _)  <|> _         = a
    Errors _  <|> b@(Ok _)  = b
    Errors as <|> Errors bs = Errors (as ++ bs)

instance MonadPlus Ok where
    mzero = empty
    mplus = (<|>)

instance Monad Ok where
    return = Ok

    Errors es >>= _ = Errors es
    Ok a      >>= f = f a

#if MIN_VERSION_base(4,9,0)
instance MonadFail Ok where
    fail str = Errors [SomeException (ErrorCall str)]

instance MonadThrow Ok where
    throwM = fail . show
#endif

-- | a way to reify a list of exceptions into a single exception

newtype ManyErrors = ManyErrors [SomeException]
   deriving (Show, Typeable)

instance Exception ManyErrors
