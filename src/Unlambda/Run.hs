{-# LANGUAGE UnicodeSyntax #-}

module Unlambda.Run
        ( run    -- run/runFile call parseLazy
        , formatParse
        , formatParseFile
        ) where

import Unlambda.AST
import Unlambda.UserInputHandler
import Unlambda.Parser
import Unlambda.Interpreter

{-
The main file for running Unlambda programs as strings.
If you wish to use the non-parsec parser, simply replace
parseLazy with parseNaive. Note that this parser is WIP
and might not work as expected.
-}

-- runs program strings
run :: String -> IO Eλ
run = showEλ . formatParse

-- passes the contents of a file to run, as a string
runFile :: String -> IO Eλ
runFile s = run =<< readFile s

-- formats a string and then passes it to the parser
formatParse :: String -> Aλ
formatParse = parseLazy . format

-- passes the contents of a file to the parser
formatParseFile :: String -> IO Aλ
formatParseFile s = formatParse <$> readFile s

