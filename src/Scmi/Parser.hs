module Scmi.Parser (parse) where

import Control.Applicative
import Data.Char (toLower)
import Text.Parsec hiding ((<|>), many, parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)

import Scmi.Types

parse :: String -> Either String Expr
parse input = case P.parse (spaces *> exprP <* eof) "" input of
    Left parseError -> Left $ show parseError
    Right expr -> Right expr

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

exprP :: Parser Expr
exprP = choice $ map try [symbolP, numberP, booleanP, listP]

symbolP :: Parser Expr
symbolP = lexeme $ (Symbol . Ident) <$> identifier
  where
    identifier = (map toLower) <$> (usual <|> pecuilar)
    usual = (:) <$> initial <*> many subsequent
    initial = letter <|> oneOf "!$%&*/:<=>?~_^"
    subsequent = initial <|> digit <|> oneOf ".+-"
    pecuilar = choice $ map string ["+", "-", "..."]

numberP :: Parser Expr
numberP = lexeme $ Number <$> integer
  where
    integer = read <$> (string "0" <|> negative <|> positive)
    negative = (:) <$> char '-' <*> positive
    positive = (:) <$> oneOf ['1'..'9'] <*> many digit

booleanP :: Parser Expr
booleanP = lexeme $ Boolean <$> bool
  where
    bool = char '#' *> (true <|> false)
    true = oneOf "Tt" *> pure True
    false = oneOf "Ff" *> pure False

listP :: Parser Expr
listP = open *> (listEnd [] <|> list [])
  where
    open = lexeme $ char '('
    close = lexeme $ char ')'
    dot = lexeme $ char '.'
    list exprs = exprP >>= \expr -> choice $ map ($ (expr:exprs)) [listEnd, improperList, list]
    listEnd exprs = close *> pure (List (reverse exprs))
    improperList exprs = dot *> (ImproperList <$> pure (reverse exprs) <*> (exprP <* close))
