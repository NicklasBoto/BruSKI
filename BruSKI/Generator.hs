{-# LANGUAGE UnicodeSyntax #-}

module Generator
        (
        ) where

---- Language Import
import AST

---- Parser Import
import Parser -- unused
import Text.Regex

---- Translation Import
import Translator

---- Table Management Import
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad

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

type Name = String

type Unlambda = String

-- type Symbol = (Name, (Bλ, [Unlambda]))
type Symbol = (Name, Bλ)

type SymbolTable = [Symbol]

getFunction :: Name -> SymbolTable -> Bλ
getFunction name table = case lookup name table of
    Just s  -> s
    Nothing -> error $ "Generator Error\nfunction " ++ name ++ " does not exist"

prependTable :: Symbol -> State SymbolTable ()
prependTable s = state $ \ss -> ((), (s:ss))

partialApply :: Integer -> Unlambda -> [Unlambda]
partialApply arity unl = take (fromInteger arity + 1) (iterate ('`':) unl)

expandExpression :: Bλ -> SymbolTable -> Bλ
expandExpression λ table = eE λ where
    eE (Idx n)         = Idx n
    eE (Abs λ)         = Abs (eE λ)
    eE (App l r)       = App (eE l) (eE r)
    eE (Fun name args) = eE $ foldl App (getFunction name table) args

-- fix overapplication, see test for the bug

generateTable :: [Stmt] -> State SymbolTable Unlambda
generateTable [] = return []
generateTable [Express λ] = generate . expandExpression λ <$> get
generateTable ((Assign name λ a):ss) = do
    curr <- get
    let resolved = expandExpression λ curr
    prependTable (name, resolved)
    generateTable ss

test = do
    putStrLn . show $ runState (generateTable . parseString $ "a := λ0\nb := λλ1\n!! a{b, b, b}") []

escapeUnl :: String -> String
escapeUnl s = case matchRegexAll (mkRegex "%[0-9]+") s of
--    Just (l, '%':i, r, _) -> l ++ (generate . translate . Idx1 . toInt) i  ++ escapeUnl r
    Just (l, '%':i, r, _) -> l ++ " ? " ++ escapeUnl r
    Nothing               -> s

-- YComb s i = "((λ (λ (1)((0)(0))) (λ (1)((0)(0)))) (λλλ ((2)(0)) ((1)(0)))) (λ0)"

