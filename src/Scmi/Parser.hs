module Scmi.Parser where

import Control.Applicative
import Text.Parsec hiding ((<|>), many, parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)

import Scmi.Types

parse :: String -> Either String Expr
parse input = case P.parse (whiteSpace *> exprP <* eof) "" input of
    Left parseError -> Left $ show parseError
    Right expr -> Right expr

exprP :: Parser Expr
exprP = choice $ map try [
    numberP, booleanP, pairP, varP, quotationP, assignmentP, definitionP,
    conditionalP, lambdaP, sequenceP, applicationP]

identP :: Parser Ident
identP = lexeme $ Ident <$> (usual <|> pecuilar)
  where
    usual = (:) <$> initial <*> many subsequent
    initial = letter <|> specialInitial
    specialInitial = oneOf "!$%&*/:<=>?~_^"
    subsequent = initial <|> digit <|> specialSubsequent
    specialSubsequent = oneOf ".+-"
    pecuilar = choice $ map string ["+", "-", "..."]

numberP :: Parser Expr
numberP = lexeme $ Number <$> integer
  where
    integer = read <$> (zeroNum <|> negativeNum <|> positiveNum)
    zeroNum = string "0"
    negativeNum = (:) <$> char '-' <*> positiveNum
    positiveNum = (:) <$> nonZeroDigit <*> many digit
    nonZeroDigit = oneOf ['1'..'9']

booleanP :: Parser Expr
booleanP = lexeme $ Boolean <$> bool
  where
    bool = char '#' *> (true <|> false)
    true = oneOf "Tt" *> pure True
    false = oneOf "Ff" *> pure False

pairP :: Parser Expr
pairP = lexeme $ Pair <$>

varP :: Parser Expr
varP = lexeme $ Var <$> identP

quotationP :: Parser Expr
quotationP = lexeme $ Quotation <$> (symbolicQuote <|> listQuote)
  where
    symbolicQuote = char '\'' *> exprP
    listQuote = sexprWith "quote" exprP

assignmentP :: Parser Expr
assignmentP = sexprWith "set!" $ Assignment <$> identP <*> exprP

definitionP :: Parser Expr
definitionP = sexprWith "define" $ Definition <$> identP <*> exprP

conditionalP :: Parser Expr
conditionalP = sexprWith "if" $ Conditional <$> exprP <*> exprP <*> exprP

lambdaP :: Parser Expr
lambdaP = sexprWith "lambda" $ Lambda <$> sexpr (many identP) <*> many exprP

sequenceP :: Parser Expr
sequenceP = sexprWith "begin" $ Sequence <$> many exprP

applicationP :: Parser Expr
applicationP = sexpr $ Application <$> exprP <*> many exprP

sexpr :: Parser a -> Parser a
sexpr = between openingParen closingParen
  where
    openingParen = lexeme $ char '('
    closingParen = lexeme $ char ')'

sexprWith :: String -> Parser Expr -> Parser Expr
sexprWith s = sexpr . (lexeme (string s) *>)

whiteSpace :: Parser ()
whiteSpace = spaces

lexeme :: Parser a -> Parser a
lexeme = (<* whiteSpace)
