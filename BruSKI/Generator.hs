{-# LANGUAGE UnicodeSyntax #-}

module Generator
        ( translate
        , generate
        , compile
        ) where

{-
I don't drink alcohol,
I drink beer.
    - Alaska State Troopers S4 E8
-}

---- Language Import
import AST
import Unlambda.AST

---- Parser Import
import Parser -- unused
import Encoding
import Text.Regex

---- Translation Import
import Translator

---- Debug Import
import Unlambda.Run -- obviously unused
import Debug.Trace -- obviously unused


{- ABSTRACTION AND FUNCTION APPLICATION

id := λ 0 :: 1
!! id{id}

-}

{- SKID HANDLER
Handle function in outside, then pass the escaped indeces in translator.

input = λ UNL{`k%0}
l = UNL{`k} => `k
r = λ0 => i
l ++ r = `ki

-}

type Symbol = (String, Bλ, [String])


escapeUnl :: String -> String
escapeUnl s = case matchRegexAll (mkRegex "%[0-9]+") s of
--    Just (l, '%':i, r, _) -> l ++ (generate . translate . Idx1 . toInt) i  ++ escapeUnl r
    Just (l, '%':i, r, _) -> l ++ " kuk " ++ escapeUnl r
    Nothing               -> s

-- YComb s i = "((λ (λ (1)((0)(0))) (λ (1)((0)(0)))) (λλλ ((2)(0)) ((1)(0)))) (λ0)"

scrape :: [Symbol] -> Stmt -> Symbol
scrape table (Assign name expr arity) = (name, sC expr, pApply (sC expr)) where
    pApply s = take (fromInteger arity + 1) (iterate ('`':) (toUnl (s)))
    sC = id
