{-# LANGUAGE UnicodeSyntax #-}

module Generator
        ( runString
        , genString
        , genFile
        , runFile
        ) where

---- Language Import
import AST
import Unlambda.AST
import Encoding
import qualified Unlambda.Run as Unlambda hiding (runFile)

---- Parser Import
import Parser
-- import Text.Regex

---- Translation Import
import Translator

---- Table Management Import
import Control.Monad.State
import Control.Monad

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

a := λ F{%0}
--> λ (F{}) (0)

-}

type Unlambda = String
type Symbol = (String, (Bλ, Int))
type SymbolTable = [Symbol]

getFunction :: String -> SymbolTable -> (Bλ, Int)
getFunction name table = case lookup name table of
    Just s  -> s
    Nothing -> error $ "Generator Error\nfunction " ++ name ++ " does not exist"

prependTable :: Symbol -> StateT SymbolTable IO ()
prependTable s = state $ \ss -> ((), (s:ss))

partialApply :: Integer -> Unlambda -> [Unlambda]
partialApply arity unl = take (fromInteger arity + 1) (iterate ('`':) unl)

-- takes care of SKID-marks
escapeFunctionIndex :: [Bλ] -> [Bλ]
escapeFunctionIndex           [] = []
escapeFunctionIndex ((Prc n):as) = (Idx n):(escapeFunctionIndex as)
escapeFunctionIndex      (a :as) =      a :(escapeFunctionIndex as)

expandExpression :: Bλ -> SymbolTable -> Bλ
expandExpression λ table = eE λ where
    eE (Idx           n)  = Idx n
    eE (Unl           s)  = Unl s
    eE (Abs (App EncZ r)) = toPrint $  show   (1 + decode r)
    eE (Abs (App EncX r)) = toPrint $ [toChar (1 + decode r)]
    eE (Abs           λ)  = Abs (eE λ)
    eE (App         l r)  = App (eE l) (eE r)
    eE (Fun   name args)  = let  (function, arity) = getFunction name table in 
                            if   length args > arity
                            then error $ "Generator Error\ntoo many arguments, arity is " ++ show arity
                            else eE $ foldl App function (escapeFunctionIndex args)
    eE (Prc           x)  = error $ "Generator Error\nnot implented: " ++ "index escape delimiters"
    eE                _   = error $ "Generator Error\nhow the hell did you get here?"

generateTable :: Sequence -> StateT SymbolTable IO Unlambda
generateTable [] = error "Generator Error\nnon-library files must terminate with an evaluation (!!)"
generateTable ((Import file):ss) = do
    lib <- lift $ parseFile file
    generateTable (lib ++ ss)

generateTable [Express λ] = generate . expandExpression λ <$> get

generateTable ((Assign name λ a):ss) = do
    curr <- get
    let resolved = expandExpression λ curr
    prependTable (name, (resolved, fromInteger a))
    generateTable ss

genString :: String -> IO (Unlambda, SymbolTable)
genString s = runStateT (generateTable . parseString $ s) []

runString :: String -> IO Eλ
runString s = do
    prog <- liftM fst . genString $ s
    Unlambda.run prog

genFile :: FilePath -> IO (Unlambda, SymbolTable)
genFile f = do
    file <- parseFile f
    runStateT (generateTable file) [] 

runFile :: FilePath -> IO Eλ
runFile f = do
    prog <- liftM fst . genFile $ f
    Unlambda.run prog

-- TODO: 
-- Text.Regex import not working
-- possibly fix muplite Express statements

{-escapeUnl :: String -> String
escapeUnl s = case matchRegexAll (mkRegex "%[0-9]+") s of
--    Just (l, '%':i, r, _) -> l ++ (generate . translate . Idx1 . toInt) i  ++ escapeUnl r
    Just (l, '%':i, r, _) -> l ++ " ? " ++ escapeUnl r
    Nothing               -> s-}

-- YComb s i = "((λ (λ (1)((0)(0))) (λ (1)((0)(0)))) (λλλ ((2)(0)) ((1)(0)))) (λ0)"
