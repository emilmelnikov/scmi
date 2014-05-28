{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scmi.Types where

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
    = SomeError
    | UserError String
    | InternalError String
    | UnboundVariable Ident
    | InvalidArgumentCount Int Int
    | InvalidApplication Expr [Expr]
    | TypeError ExprType Expr
    deriving (Show)

instance Error ScmiError where
    noMsg = SomeError
    strMsg = UserError

data Expr
    -- Self-evaluating
    = Number Integer
    | Boolean Bool
    | Pair Expr Expr
    | EmptyList
    | Unspecified
    -- Variable
    | Var Ident
    -- Special forms
    | Quotation Expr
    | Assignment Ident Expr
    | Definition Ident Expr
    | Conditional Expr Expr Expr
    | Lambda [Ident] [Expr]
    | Sequence [Expr]
    -- Application
    | Application Expr [Expr]
    -- Other
    | Procedure [Ident] Expr Env
    | Primitive ([Expr] -> Scmi Expr)

instance Show Expr where
    show e = case e of
        Number num -> show num
        Boolean bool -> if bool then "#t" else "#f"
        Pair car cdr -> mkList $ reverse $ showPair cdr [show car]
        EmptyList -> "()"
        Unspecified -> "#<void>"
        Var name -> show name
        Quotation expr -> mkList ["quote", show expr]
        Assignment name expr -> mkList ["set!", show name, show expr]
        Definition name expr -> mkList ["define", show name, show expr]
        Conditional cond conseq alt -> mkList ["if", show cond, show conseq, show alt]
        Lambda args body -> mkList $ "lambda" : mkList (map show args) : map show body
        Sequence exprs -> mkList $ "begin" : map show exprs
        Application proc args -> mkList $ show proc : map show args
        Procedure _ _ _ -> procedureRepr
        Primitive _ -> procedureRepr
      where
        showPair cdr acc = case cdr of
            Pair cadr cddr -> showPair cddr (show cadr : acc)
            EmptyList -> acc
            expr -> show expr : "." : acc
        mkList ss = "(" ++ intercalate " " ss ++ ")"
        procedureRepr = "#<procedure>"

data ExprType
    = BooleanType
    | PairType
    | SymbolType
    | NumberType
    | CharacterType
    | StringType
    | VectorType
    | ProcedureType
    | NoType

instance Show ExprType where
    show et = case et of
        BooleanType -> "boolean"
        PairType -> "pair"
        SymbolType -> "symbol"
        NumberType -> "number"
        CharacterType -> "char"
        StringType -> "string"
        VectorType -> "vector"
        ProcedureType -> "procedure"
        NoType -> "other"

typeOf :: Expr -> ExprType
typeOf e = case e of
    Boolean _ -> BooleanType
    Number _ -> NumberType
    Pair _ _ -> PairType
    Procedure _ _ _ -> ProcedureType
    Primitive _ -> ProcedureType
    _ -> NoType

type Frame = M.HashMap Ident (IORef Expr)

newtype Env = Env { unEnv :: [Frame] }

newtype Scmi a = Scmi { unScmi :: ErrorT ScmiError (ReaderT Env IO) a }
    deriving (Functor, Monad, MonadError ScmiError, MonadReader Env, MonadIO)
