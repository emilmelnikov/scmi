{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scmi.Types where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.List

newtype Ident = Ident { unIdent :: String }
    deriving (Eq, Hashable)

instance Show Ident where
    show = unIdent

data ScmiError
    -- Default error
    = SomeError
    -- Generic error with message
    | UserError String
    -- Error that should never occur
    | InternalError String
    -- Variable is not present in any frame
    | UnboundVariable Ident
    -- Compound procedure arity mismatch
    | InvalidArgumentCount Int Int
    -- Expression is not valid in some context
    | SyntaxError Expr
    -- Special form invocation is invalid
    | InvalidSpecialForm [Expr]
    -- Procedure application is invalid
    | InvalidApplication Expr [Expr]
    -- Value of invalid type is supplied to procedure
    | TypeError ExprType Expr
    deriving (Show)

instance Error ScmiError where
    noMsg = SomeError
    strMsg = UserError

emptyImproperList :: Expr -> ScmiError
emptyImproperList e = InternalError $ "empty improper list: " ++ show e

data Expr
    = Symbol Ident
    | Number Integer
    | Boolean Bool
    | List [Expr]
    | ImproperList [Expr] Expr
    | Unspecified
    | Procedure [Ident] (Maybe Ident) [Expr] Env
    | Primitive ([Expr] -> Scmi Expr)

showExpr :: Expr -> String
showExpr e = case e of
    Symbol sym -> show sym
    Number num -> show num
    Boolean bool -> if bool then "#t" else "#f"
    List exprs -> mkList $ map show exprs
    ImproperList exprs end -> mkList $ map show exprs ++ ["." , show end]
    Unspecified -> "#<void>"
    Procedure _ _ _ _ -> procedureRepr
    Primitive _ -> procedureRepr
  where
    mkList ss = "(" ++ intercalate " " ss ++ ")"
    procedureRepr = "#<procedure>"

instance Show Expr where
    show = showExpr

cons :: Expr -> Expr -> Expr
cons car cdr = case cdr of
    List exprs -> List (car : exprs)
    ImproperList exprs end -> ImproperList (car : exprs) end
    _ -> ImproperList [car] cdr

data ExprType
    = SymbolType
    | NumberType
    | BooleanType
    | CharacterType
    | StringType
    | VectorType
    | PairType
    | ProcedureType
    | UnspecifiedType

instance Show ExprType where
    show et = case et of
        SymbolType -> "symbol"
        NumberType -> "number"
        BooleanType -> "boolean"
        CharacterType -> "char"
        StringType -> "string"
        VectorType -> "vector"
        PairType -> "pair"
        ProcedureType -> "procedure"
        UnspecifiedType -> "unspecified"

typeOf :: Expr -> ExprType
typeOf e = case e of
    Symbol _ -> SymbolType
    Number _ -> NumberType
    Boolean _ -> BooleanType
    List _ -> PairType
    ImproperList _ _ -> PairType
    Procedure _ _ _ _ -> ProcedureType
    Primitive _ -> ProcedureType
    Unspecified -> UnspecifiedType

type Frame = M.HashMap Ident (IORef Expr)

newtype Env = Env { unEnv :: [Frame] }

newtype Scmi a = Scmi { unScmi :: ErrorT ScmiError (ReaderT (IORef Env) IO) a }
    deriving (Functor, Applicative, Monad, MonadError ScmiError, MonadReader (IORef Env), MonadIO)

runScmi :: Scmi a -> IORef Env -> IO (Either ScmiError a)
runScmi = runReaderT . runErrorT . unScmi
