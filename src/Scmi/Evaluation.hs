module Scmi.Evaluation where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import qualified Data.HashMap.Strict as M

import Scmi.Types

findBinding :: Ident -> Scmi (IORef Expr)
findBinding name = ask >>= liftIO . readIORef >>= findBindingRec . unEnv
  where
    findBindingRec fs = case fs of
        [] -> throwError $ UnboundVariable name
        frame:frames -> maybe (findBindingRec frames) return $ M.lookup name frame

getBinding :: Ident -> Scmi Expr
getBinding name = findBinding name >>= liftIO . readIORef

updateBinding :: Ident -> Expr -> Scmi ()
updateBinding name expr = findBinding name >>= liftIO . (`writeIORef` expr)

addBinding :: Ident -> Expr -> Scmi ()
addBinding name expr = do
    envRef <- ask
    frames <- liftIO $ unEnv <$> readIORef envRef
    case frames of
        [] -> throwError $ InternalError "empty frame stack"
        f:fs -> do
            exprRef <- liftIO $ newIORef expr
            liftIO $ writeIORef envRef $ Env $ (M.insert name exprRef f) : fs

eval :: Expr -> Scmi Expr
eval e = case e of
    Symbol sym -> getBinding sym
    List exprs -> evalList exprs
    ImproperList _ _ -> throwError $ UserError $ "can't evaluate improper list: " ++ show e
    _ -> return e

evalList :: [Expr] -> Scmi Expr
evalList [] = throwError $ UserError "empty list"
evalList es@(sym:body) = case sym of
    Symbol (Ident "quote") -> evalQuotation
    Symbol (Ident "quasiquote") -> evalQuasiquote
    Symbol (Ident "unquote") -> evalUnquote
    Symbol (Ident "unquote-splicing") -> evalUnquoteSplicing
    Symbol (Ident "set!") -> evalAssignment
    Symbol (Ident "define") -> evalDefinition
    Symbol (Ident "if") -> evalConditional
    Symbol (Ident "lambda") -> evalLambda
    Symbol (Ident "begin") -> evalSequence body
    _ -> evalProcedure
  where
    evalQuotation = case body of
        [expr] -> return expr
        _ -> listSyntaxError
    evalQuasiquote = case body of
        [expr] -> return expr
        _ -> listSyntaxError
    evalUnquote = case body of
        [expr] -> return expr
        _ -> listSyntaxError
    evalUnquoteSplicing = case body of
        [expr] -> return expr
        _ -> listSyntaxError
    evalAssignment = case body of
        [Symbol name, expr] -> eval expr >>= updateBinding name >> return Unspecified
        _ -> listSyntaxError
    evalDefinition = case body of
        [Symbol name, expr] -> eval expr >>= addBinding name >> return Unspecified
        _ -> listSyntaxError
    evalConditional = case body of
        [cond, conseq] -> eval cond >>= evalIf conseq Unspecified
        [cond, conseq, alt] -> eval cond >>= evalIf conseq alt
        _ -> listSyntaxError
    evalLambda = case body of
        List args : body@(_:_) -> mkProc args Nothing body
        _ -> listSyntaxError
    evalProcedure = do
        proc <- eval sym
        args <- mapM eval body
        apply proc args
    listSyntaxError = throwError $ SyntaxError $ List es
    evalIf t f p = eval $ if isTrue p then t else f
    isTrue e = case e of
        Boolean False -> False
        Unspecified -> False
        _ -> True
    mkProc args vararg body =
        Procedure <$> mapM toIdent args <*> pure vararg <*> pure body <*> (ask >>= liftIO . readIORef)
    toIdent e = case e of
        Symbol ident -> return ident
        _ -> throwError $ SyntaxError e

evalSequence :: [Expr] -> Scmi Expr
evalSequence es = case es of
    [] -> return Unspecified
    [expr] -> eval expr
    expr:exprs -> eval expr >> evalSequence exprs

apply :: Expr -> [Expr] -> Scmi Expr
apply proc actuals = case proc of
    Procedure formals mvararg body env -> do
        frame <- makeFrame formals actuals
        runScmiLocal (evalSequence body) (extendEnv frame env)
    Primitive prim -> prim actuals
    _ -> throwError $ InvalidApplication proc actuals
  where
    extendEnv frame = Env . (frame :) . unEnv

runScmiLocal :: Scmi a -> Env -> Scmi a
runScmiLocal scmi env = liftIO (newIORef env) >>= liftIO . runScmi scmi >>= either throwError return

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
