{-# LANGUAGE UnicodeSyntax #-}

module ErrorHandler
        (
        ) where

data ParserError   = Missing | Unknown deriving Show
data CompilerError = AbstractDangle | FreeVars deriving Show

-- Hopefully handle, define, and show errors here