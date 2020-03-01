module Test where

import Unlambda.AST
import Unlambda.Run
import Expression.Parser
import Expression.DeBruijn

int :: String -> IO Eλ
int = eval . parseExpression

eval :: Bλ -> IO Eλ
eval (Unl s) = run s
