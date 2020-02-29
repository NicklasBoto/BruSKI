{-# LANGUAGE UnicodeSyntax #-}

module Unlambda.Parser
        ( parseLazy
        , parseNaive
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
tryEλ = try (char '`' *> (A     <$> tryEλ <*>    tryEλ)) <|>
        try (char '.' *> (E . D <$> fmap (:[]) anyChar)) <|>
        try (anyChar >>= (return . getA))             -- <|> symmetry is nice!

-----------------------------------------------------------------------
--------------------------- Naive Parse  ------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            Parsing is hell
            This is dumb
            So am I
        ---*-_-*-_-*-_-*---}

-- A (A (E $ D "h") (E $ D "m")) (E I)
-- `  ` [E $ D "h" , E $ D "m" ,  E I]
-- ` (`     (.  h)      (.  m))    (i)
-- `  `      .  h        .  m       i
-- ``.h.mi

parseNaive :: String -> Aλ
parseNaive = parseA' . reverse . parseE

parseA' :: [Aλ] -> Aλ
parseA' []     = error "parseA (1): Invalid program"
parseA' [e]    = e
parseA' (e:es) = flip A e $ parseA' es

test :: String -> Aλ
test s = flip parseA s . reverse . parseE $ s

-- Parses the λ-applications
parseA :: [Aλ] -> String -> Aλ
parseA [] _     = error "(1) free expression"
parseA _ []     = error "(2) non application"
parseA _ [_]    = error "(3) free application"
parseA [e] _    = e
parseA (e:es) (a:as) = case a of
                         '`' -> flip A e $ parseA es as
                         _   -> parseA (e:es) as

-- Parses the λ-expressions
parseE :: String -> [Aλ]
parseE []         = []
parseE [a]        = case a of
                      'i' -> [E I]
                      'r' -> [E R]
                      _   -> error "parseE (1): Invalid program. Free nullary."

parseE [a,b]   = case a of
                     '`' -> parseE [b]
                     '.' -> [E $ D [b]]
                     'i' -> E I : parseE [b]
                     'r' -> E R : parseE [b]
                     'k' -> E K : parseE [b]
                     's' -> E S : parseE [b]
                     _   -> error "parseE (2): Invalid program. Free unary."
parseE (a:b:cs) = case a of
                     '`' -> parseE (b:cs)
                     '.' -> E (D [b]) : parseE cs
                     'i' -> E I : parseE (b:cs)
                     'r' -> E R : parseE (b:cs)
                     'k' -> E K : parseE (b:cs)
                     's' -> E S : parseE (b:cs)
                     _   -> error "parseE (3): Invalid program. Free polyary."

-- WIP
parseSK :: String -> [Aλ]
parseSK []       = []
parseSK [a]   = case a of
                       's' -> [E S]
                       'k' -> [E K]
parseSK [a,b] = case a of
                       's' -> [E $ Sf (getE b)]
                       'k' -> [E $ Kf (getE b)]
parseSK [a,b,c] = case a of
                       's' -> [E $ Sff (getE b) (getE c)]
parseSK (a:b:c:ds) = case a of
                       's' -> E (Sff (getE b) (getE c)) : parseSK ds
                       'k' -> E (Kf (getE b)) : parseSK (c:ds)

-- Might use a hash table for this later
getE :: Char -> Eλ
getE 's' = S
getE 'k' = K
getE 'i' = I
getE 'r' = R
getE 'v' = V
getE  c  = error $ "\nParse Error in \"getE\":\nexpecting \"s\", \"k\", \"i\", or \"r\"\ngot " ++ [c]

getA :: Char -> Aλ
getA = E . getE

