{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module MacroHandler
        ( parseMacro
        , parseMacros
        ) where

---- Format Import
import Main.Utf8       (withUtf8)

---- Helper Import
import Data.List
import Data.List.Extra (replace)
import Data.List.Split (splitOn)
import Data.Bifunctor  (first)
import Data.Maybe

---- Parsec Import
import Text.ParserCombinators.Parsec

---- Language Import
import Generator 
import Parser
import Config
import Lexer
import AST

---- Table Managent Import
import Control.Monad.State
import Control.Monad


---- Macro Constructor
type MacroGen  = [String] -> String
type ArgFixity = [(Char, Int)]

instance Show MacroGen where
        show = const "__MACROGEN__"

instance {-# OVERLAPPING #-} Show [Macro] where
        show = intercalate "\n" . map show

data Macro = Macro
    { symbol :: String
    , args   :: ArgFixity
    , def    :: String
    , gen    :: MacroGen
    } deriving Show


---- Symbol Parsers
type MacroTable = [Macro]

creepMacros :: [String] -> MacroTable -> Parser String
creepMacros prev env = do
        mN <- identifier
        case macroLookup mN env of
          Just Macro {..} -> gen <$> getArgs args
          Nothing         -> return mN

macroLookup :: String -> MacroTable -> Maybe Macro
macroLookup var env = case filter ((==var).symbol) env of
                    [m] -> Just m
                    []  -> Nothing
                    _   -> error "Macro Error\nmultiple macro definitions"

getArgs :: ArgFixity -> Parser [String]
getArgs args = count (length args) identifier 

---- Macro Definiton Parsers
macroBegin, macroEnd :: Parser String
macroDelim = '+'     :: Char
macroBegin = string ['{',macroDelim]
macroEnd   = string [macroDelim,'}']

whileMacro :: Parser MacroTable
whileMacro = manyTill macroParser eof

macroParser :: Parser Macro
macroParser = do
        (name, args) <- macroHeaderParser
        def <- wrapSpace macroDefParser
        let exp = makeMacro args def
        return $ Macro name args def exp

makeMacro :: ArgFixity -> String -> MacroGen
makeMacro _   "" = const ""
makeMacro args l = \s -> f s args l where
        f []     []         l = l
        f (x:xs) ((a,_):as) l = f xs as $ replace ['$',a] x l
        f _      _          _ = error $ "Macro Error\nincorrect number of macro arguments\narity is " ++ show (length args)

macroHeaderParser :: Parser (String, ArgFixity)
macroHeaderParser = do
        wrapSpace macroBegin
        info <- manyTill headerInfo (char ';')
        return $ argumentFixity info

argumentFixity :: [String] -> (String, ArgFixity)
argumentFixity t  = (name, args) where
        args      =  map (first last) $ delete (name,0) (zip t (map (subtract ix) [0..]))
        (name,ix) = case find ((/='$') . head) t of
                     Just nm -> (nm, fromJust (elemIndex name t))
                     Nothing -> error "Macro Error\nmissing identifier"
        
headerInfo :: Parser String
headerInfo =  identifier <|> macroArg

macroArg :: Parser String
macroArg = do
        char '$'
        id <- allChars
        return ['$', id]

macroDefParser :: Parser String
macroDefParser = manyTill anyChar (try macroEnd)


---- Runner
parseSymbols :: MacroTable -> String -> String
parseSymbols env str = case parse (creepMacros [] env) "Macro Error" str of
                   Left  e -> error $ show e
                   Right r -> r

parseMacro :: String -> Macro
parseMacro str = case parse macroParser "Macro Error" str of
                   Left  e -> error $ show e
                   Right r -> r

parseMacros :: FilePath -> IO [Macro]
parseMacros file = withUtf8 $ do
        macro <- readFile file
        case parse whileMacro file macro of
          Left  e -> print e >> fail "Macro Error"
          Right r -> return r

