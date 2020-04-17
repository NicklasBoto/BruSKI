{-# LANGUAGE UnicodeSyntax #-}

module Generator
        (
        ) where

---- Language Import
import AST
import Unlambda.AST
import Encoding

---- Parser Import
import Parser -- unused
-- import Text.Regex

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
type Symbol = (Name, (Bλ, Int))

type SymbolTable = [Symbol]

getFunction :: Name -> SymbolTable -> (Bλ, Int)
getFunction name table = case lookup name table of
    Just s  -> s
    Nothing -> error $ "Generator Error\nfunction " ++ name ++ " does not exist"

prependTable :: Symbol -> StateT SymbolTable IO ()
prependTable s = state $ \ss -> ((), (s:ss))

partialApply :: Integer -> Unlambda -> [Unlambda]
partialApply arity unl = take (fromInteger arity + 1) (iterate ('`':) unl)

expandExpression :: Bλ -> SymbolTable -> Bλ
expandExpression λ table = eE λ where
    eE (Idx           n)  = Idx n
    eE (Unl           s)  = Unl s
    eE (Abs (App EncZ r)) = toPrint $  show (1 + decode r)
    eE (Abs (App EncX r)) = toPrint $ [toChar (1 + decode r)]
    eE (Abs           λ)  = Abs (eE λ)
    eE (App         l r)  = App (eE l) (eE r)
    eE (Fun   name args)  = let  (function, arity) = getFunction name table in 
                            if   length args > arity
                            then error $ "Generator Error\ntoo many arguments, arity is " ++ show arity
                            else eE $ foldl App function args

generateTable :: [Stmt] -> StateT SymbolTable IO Unlambda
generateTable [] = error "Generator Error\nnon-library files must terminate with an evaluation (!!)"
generateTable ((Import file):ss) = do
    lib <- lift $ parseFile ("Prelude/" ++ file ++ ".bru")
    generateTable (lib ++ ss)

generateTable [Express λ] = generate . expandExpression λ <$> get

generateTable ((Assign name λ a):ss) = do
    curr <- get
    let resolved = expandExpression λ curr
    prependTable (name, (resolved, fromInteger a))
    generateTable ss

genString :: String -> IO (Unlambda, SymbolTable)
genString s = runStateT (generateTable . parseString $ s) []

genUnl :: String -> IO Unlambda
genUnl = liftM fst . genString

runString :: String -> IO Eλ
runString s = do
    prog <- genUnl s
    run prog

comb :: String
comb = "S := λλλ ((2)(0)) ((1)(0)) :: 3\nK := λλ 1                   :: 2\nI := λ (S{K, K}) (0)        :: 1\n!! I"

-- TODO: 
-- utf8 issues with parseFile
-- Text.Regex import not working
-- possibly fix muplite Express statements

{-escapeUnl :: String -> String
escapeUnl s = case matchRegexAll (mkRegex "%[0-9]+") s of
--    Just (l, '%':i, r, _) -> l ++ (generate . translate . Idx1 . toInt) i  ++ escapeUnl r
    Just (l, '%':i, r, _) -> l ++ " ? " ++ escapeUnl r
    Nothing               -> s-}

-- YComb s i = "((λ (λ (1)((0)(0))) (λ (1)((0)(0)))) (λλλ ((2)(0)) ((1)(0)))) (λ0)"

