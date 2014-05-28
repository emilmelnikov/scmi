module Scmi.Evaluation where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import qualified Data.HashMap.Strict as M

import Scmi.Types

runScmi :: Scmi a -> Env -> IO (Either ScmiError a)
runScmi = runReaderT . runErrorT . unScmi

findBinding :: Ident -> Scmi (IORef Expr)
findBinding name = ask >>= findBindingRec . unEnv
  where
    findBindingRec env = case env of
        [] -> throwError $ UnboundVariable name
        frame:fs -> maybe (findBindingRec fs) return $ M.lookup name frame

getBinding :: Ident -> Scmi Expr
getBinding name = findBinding name >>= liftIO . readIORef

updateBinding :: Ident -> Expr -> Scmi ()
updateBinding name expr = findBinding name >>= liftIO . (`writeIORef` expr)

isTrue :: Expr -> Bool
isTrue expr = case expr of
    Boolean False -> False
    Unspecified -> False
    _ -> True

eval :: Expr -> Scmi Expr
eval e = case e of
    Number _ -> return e
    Boolean _ -> return e
    Pair _ _ -> return e
    EmptyList -> return e
    Unspecified -> return e
    Var name -> getBinding name
    Quotation expr -> return expr
    Assignment name expr -> eval expr >>= updateBinding name >> return Unspecified
    Definition name expr -> undefined
    Conditional cond conseq alt -> eval cond >>= \p -> eval $ if isTrue p then conseq else alt
    Lambda args body -> Procedure args (Sequence body) <$> ask
    Sequence exprs -> evalSequence exprs
    Application proc args -> eval proc >>= \proc' -> mapM eval args >>= \args' -> apply proc' args'
    Procedure _ _ _ -> return e
    Primitive _ -> return e
  where
    evalSequence exprs = case exprs of
        [] -> return Unspecified
        [ex] -> eval ex
        ex:exs -> eval ex >> evalSequence exs

makeFrame :: [Ident] -> [Expr] -> Scmi Frame
makeFrame formals actuals
    | formalsCount /= actualsCount =
        throwError $ InvalidArgumentCount formalsCount actualsCount
    | otherwise = do
        actualsRefs <- forM actuals $ liftIO . newIORef
        return $ M.fromList (zip formals actualsRefs)
    where
      formalsCount = length formals
      actualsCount = length actuals

rewrapScmi :: Scmi a -> Env -> Scmi a
rewrapScmi scmi env = liftIO (runScmi scmi env) >>= either throwError return

apply :: Expr -> [Expr] -> Scmi Expr
apply proc actuals = case proc of
    Procedure formals body env -> do
        frame <- makeFrame formals actuals
        rewrapScmi (eval body) (extendEnv frame env)
    Primitive prim -> prim actuals
    _ -> throwError $ InvalidApplication proc actuals
  where
    extendEnv frame = Env . (frame :) . unEnv
