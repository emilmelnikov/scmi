module Scmi.Interpreter where

import Data.Char
import System.IO

import Scmi.Evaluation
import Scmi.Parser
import Scmi.Primitives
import Scmi.Types

repl :: IO ()
repl = do
    envRef <- globalEnv
    replEnv envRef
  where
    replEnv envRef = do
        input <- fmap (dropWhile isSpace) $ hPutStr stdout "scmi> " >> hFlush stdout >> hGetLine stdin
        case input of
            "" -> replEnv envRef
            "\EOT" -> return ()
            _ -> case parse input of
                Left err -> putStrLn err >> replEnv envRef
                Right parsedExpr -> runScmi (eval parsedExpr) envRef >>= either print print >> replEnv envRef
