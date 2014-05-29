module Scmi.Primitives (globalEnv) where

import Control.Applicative
import Control.Monad.Error
import qualified Data.HashMap.Strict as M
import Data.IORef

import Scmi.Types

toNumber :: Expr -> Scmi Integer
toNumber e = case e of
    Number num -> return num
    _ -> throwError $ TypeError NumberType e

foldNum :: (Integer -> Integer -> Integer) -> Integer -> [Expr] -> Scmi Expr
foldNum f z args = (Number . foldr f z) <$> (mapM toNumber args)

globalEnv :: IO (IORef Env)
globalEnv = mkEnv =<< mapM mkPrim [
    ("+", foldNum (+) 0),
    ("*", foldNum (*) 1)]
  where
    mkEnv = newIORef . Env . (: []) . M.fromList
    mkPrim (name, f) = (,) <$> pure (Ident name) <*> newIORef (Primitive f)
