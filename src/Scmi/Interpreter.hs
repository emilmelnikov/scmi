module Scmi.Interpreter where

import Scmi.Evaluation
import Scmi.Parser
import Scmi.Primitives

run :: String -> IO ()
run s = case parse s of
    Left err -> putStrLn err
    Right expr -> globalEnv >>= runScmi (eval expr) >>= either print print
