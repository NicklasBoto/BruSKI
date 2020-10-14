{-# LANGUAGE UnicodeSyntax #-}

module ErrorHandler
        ( failWith
        ) where

data ParserError   = Missing | Unknown deriving Show
data CompilerError = AbstractDangle | FreeVars deriving Show

failWith e = errorWithoutStackTrace $ "*** " ++ e

-- Hopefully handle, define, and show errors here
