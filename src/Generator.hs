{-# LANGUAGE UnicodeSyntax #-}

module Generator
        ( runString
        , genString
        , genFile
        , runFile
        , generateTable
        , expandExpression
        ) where

---- Language Import
import AST
import Unlambda.AST
import Encoding
import Config
import ErrorHandler
import qualified Unlambda.Run as Unlambda hiding (runFile)

---- Parser Import
import Parser
-- import Text.Regex

---- Translation Import
import Translator

---- Table Management Import
import Control.Monad.State
import Control.Monad

getFunction :: String -> SymbolTable -> (Bλ, Int)
getFunction name table = case lookup name table of
    Just s  -> s
    Nothing -> failWith $ "Generator Error\nfunction " ++ name ++ " does not exist"

prependTable :: Symbol -> StateT SymbolTable IO ()
prependTable s = state $ \ss -> ((), s:ss)

partialApply :: Integer -> Unlambda -> [Unlambda]
partialApply arity unl = take (fromInteger arity + 1) (iterate ('`':) unl)

expandExpression :: Bλ -> SymbolTable -> Bλ
expandExpression λ table = eE λ where
    eE (Idx             n)  = Idx n
    eE (Unl             s)  = Unl s
    eE (Abs             λ)  = Abs (eE λ)
    eE (App         e l r)  = if   e 
                              then reduceToNormal $ App e (eE l) (eE r)
                              else                  App e (eE l) (eE r)
    eE (Fun "__EQ__" args)  = let xs = map (reduceToNormal . eE) args in
                              if   all (== head xs) (tail xs)
                              then Abs $ Abs (Idx 1)
                              else Abs $ Abs (Idx 0)
    eE (Fun     name args)  = let  (function, arity) = getFunction name table in 
                              if   length args > arity
                              then failWith $ "Generator Error\ntoo many arguments, " ++ name ++ " has arity " ++ show arity
                              else eE $ foldl (App True) function args

generateTable :: Sequence -> StateT SymbolTable IO Unlambda
generateTable [] = failWith "Generator Error\nnon-library files must terminate with an evaluation (!!)"
generateTable ((Import file):ss) = do
    lib <- lift $ parseFile (Config.preludePath ++ file ++ ".bru")
    generateTable (lib ++ ss)

generateTable [Express λ] = do
    table <- get
    let formatλ = case lookup "__FORMATTER__" table of
                        Just (f,_) -> App True f λ
                        Nothing    -> λ

    return $ generate (expandExpression formatλ table)

generateTable ((Assign name λ a):ss) = do
    curr <- get
    let resolved = expandExpression λ curr
    prependTable (name, (resolved, fromInteger a))
    generateTable ss

genString :: String -> IO (Unlambda, SymbolTable)
genString s = runStateT (generateTable . parseString $ s) []

runString :: String -> IO Eλ
runString s = do
    prog <- fmap fst . genString $ s
    Unlambda.run prog

genFile :: FilePath -> IO (Unlambda, SymbolTable)
genFile f = do
    file <- parseFile f
    runStateT (generateTable file) [] 

runFile :: FilePath -> IO Eλ
runFile f = do
    prog <- fmap fst . genFile $ f
    Unlambda.run prog

-- TODO possibly fix muplite Express statements
-- FIXME or not, nah
