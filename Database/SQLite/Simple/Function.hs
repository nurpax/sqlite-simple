{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.SQLite.Simple.Function
    (
      Function
    , createFunction
    , deleteFunction
    ) where

import Control.Exception
import Data.Proxy
import Database.SQLite3 as Base hiding (createFunction,deleteFunction,funcArgText,funcResultText)
import qualified Database.SQLite3.Direct as Base
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

class Function a where
  argCount :: Proxy a -> Int
  deterministicFn :: Proxy a -> Bool
  evalFunction :: Base.FuncContext -> Base.FuncArgs -> Int -> a -> IO ()

instance {-# OVERLAPPING #-} (ToField a) => Function a where
  argCount = const 0
  deterministicFn = const True
  evalFunction ctx _ _ a = case toField a of
    SQLInteger r -> Base.funcResultInt64 ctx r
    SQLFloat r -> Base.funcResultDouble ctx r
    SQLText r -> Base.funcResultText ctx $ Base.Utf8 $ TE.encodeUtf8 r
    SQLBlob r -> Base.funcResultBlob ctx r
    SQLNull -> Base.funcResultNull ctx

instance {-# Overlapping #-} (Function a) => Function (IO a) where
  argCount = const 0
  deterministicFn = const False
  evalFunction ctx args ca a = a >>= evalFunction ctx args ca

instance {-# Overlapping #-} forall f r . (Function r, FromField f) => Function (f -> r) where
  argCount = const $ argCount (Proxy :: Proxy r) + 1
  deterministicFn = const $ deterministicFn (Proxy :: Proxy r)
  evalFunction ctx args ca fn = let ca' = Base.ArgCount ca in do
    sqlv <- Base.funcArgType args ca' >>= \ct -> case ct of
      Base.IntegerColumn -> SQLInteger <$> Base.funcArgInt64 args ca'
      Base.FloatColumn -> SQLFloat <$> Base.funcArgDouble args ca'
      Base.TextColumn -> (\(Base.Utf8 b) -> SQLText $ TE.decodeUtf8 b) <$>
        Base.funcArgText args ca'
      Base.BlobColumn -> SQLBlob <$> Base.funcArgBlob args ca'
      Base.NullColumn -> pure SQLNull
    case fromField $ Field sqlv ca of
      Ok arg -> evalFunction ctx args (ca + 1) (fn arg)
      Errors ex -> throw $ ManyErrors ex

createFunction :: forall f . Function f => Connection -> T.Text -> f -> IO (Either Base.Error ())
createFunction conn fn f = Base.createFunction
  (connectionHandle conn)
  (Base.Utf8 $ TE.encodeUtf8 fn)
  (Just $ Base.ArgCount $ argCount (Proxy :: Proxy f))
  (deterministicFn (Proxy :: Proxy f))
  (\ctx args -> catch
    (evalFunction ctx args 0 f)
    ((const :: IO () -> SomeException -> IO ()) $ Base.funcResultNull ctx))

deleteFunction :: Connection -> T.Text -> IO (Either Base.Error ())
deleteFunction conn fn = Base.deleteFunction
  (connectionHandle conn)
  (Base.Utf8 $ TE.encodeUtf8 fn)
  Nothing
