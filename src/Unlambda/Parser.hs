{-# LANGUAGE UnicodeSyntax #-}

module Unlambda.Parser
        ( parseLazy
        , getE
        , getA
        ) where

import Text.Parsec
import Unlambda.AST

-----------------------------------------------------------------------
-------------------------- Parsec Parse  ------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            What giving up is like
            This is
        ---*-_-*-_-*-_-*---}

parseLazy :: String -> Aλ
parseLazy = handle . parse tryEλ "parseLazy"

-- Handles errors in the parser.
handle :: Either ParseError Aλ -> Aλ
handle = either (error . ("\nParse Error in " ++) . show) id

-- Parses program string to the application-AST.
-- The anyChar tries to match some character to a
-- λ-expression.
tryEλ = try (char '`' *> (A       <$> tryEλ <*>    tryEλ)) <|>
        try (char '.' *> (E . Dot <$> fmap (:[]) anyChar)) <|>
        try (anyChar >>= (return . getA))             -- <|> symmetry is nice!

-- Might use a hash table for this later
getE :: Char -> Eλ
getE 's' = S
getE 'k' = K
getE 'i' = I
getE 'r' = R
getE 'v' = V
getE 'd' = D
getE 'e' = Ex
getE  c  = error $ "\nParse Error in \"getE\":\nexpecting \"s\", \"k\", \"i\", or \"r\"\ngot " ++ [c]

getA :: Char -> Aλ
getA = E . getE

