{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lib
import Generator
import Unlambda.AST (Eλ)
import Unlambda.Run (run)
import AST
import Encoding
import Parser
import Turtle
import Paths_BruSKI (version)
import Data.Version (showVersion)
import System.Console.Repline
import Control.Monad
import Data.Functor (($>))
import Control.Monad.State
import Data.List
import Data.Char
import Config
-- import MacroHandler
import System.IO
import Text.Parsec.Error (ParseError)
import System.Directory
import System.Console.Haskeline.MonadException
import qualified Sexy

-- main parser
mainSubroutine :: IO ()
mainSubroutine = Sexy.welcome

parseMain :: Parser (IO ())
parseMain = pure mainSubroutine

-- compile parser
getOutputPath :: Parser (Turtle.FilePath, Maybe Turtle.FilePath)
getOutputPath = (,) <$> argPath "source" "Input file of compiler"
                    <*> optional (optPath "target" 'o' "Output file of compiler")

compileToFile :: Turtle.FilePath -> Maybe Turtle.FilePath -> IO ()
compileToFile source Nothing = writeToFile (encodeString source) (generateFileName ".")
compileToFile source (Just target)
  | filename target == emptyPath = writeToFile fSource (generateFileName fTarget)
  | otherwise                    = writeToFile fSource                   fTarget
    where fSource   = encodeString source
          fTarget   = encodeString target
          emptyPath = decodeString ""

compString :: String
compString = "### GENERATED BY THE BRUSKI COMPILER ###\n"

writeToFile :: System.IO.FilePath -> System.IO.FilePath -> IO ()
writeToFile source target = do
        file <- openFile target WriteMode 
        compiled <- fmap fst . genFile $ source
        hPutStrLn file (compString ++ compiled)
        hClose file

-- generateFileName :: System.IO.FilePath -> String
generateFileName folder = "out.unl"

compileParser :: Parser (IO ())
compileParser = fmap (uncurry compileToFile) (subcommand "compile" "Compile BruSKI source code to Unlambda file" getOutputPath)

-- run parser
getInputPath :: Parser Turtle.FilePath
getInputPath = argPath "source" "Input file of compiler"

runFromFile :: Turtle.FilePath -> IO ()
runFromFile source = print =<< Generator.runFile (encodeString source)

runParser :: Parser (IO ())
runParser = fmap runFromFile (subcommand "run" "Run BruSKI source code without saving it to a file" getInputPath)

-- gen parser
genFromFile :: Turtle.FilePath -> IO ()
genFromFile source = do
        (unl, table) <- Generator.genFile (encodeString source)
        putStrLn "--- symbol table at compile time"
        print $ reverse table
        putStrLn "\n--- compiler output"
        print unl

genParser :: Parser (IO ())
genParser = fmap genFromFile (subcommand "gen" "Generate symbol table and compiler output to terminal" getInputPath)

-- version parser
version' :: IO()
version' = putStrLn (showVersion version)

verboseVersion :: IO()
verboseVersion = do
                 version'
                 echo "This compiler is currently in development! Please notify me at [mail@nicklasbotö.se], or open an issue at [bruski.nicklasbotö.se] if you want a certain feature."

parseVersion :: Parser (IO ())
parseVersion = subcommand "version" "Show compiler version" (pure verboseVersion)

-- Bruce Wayne parser
bat = [ "       _,    _   _    ,_       "
      , "  .o888P     Y8o8Y     Y888o.  "
      , " d88888      88888      88888b "
      , "d888888b_  _d88888b_  _d888888b"
      , "8888888888888888888888888888888"
      , "8888888888888888888888888888888"
      , "YJGS8PYY888PYY888PYY888PYY8888P"
      , " Y888   '8'   Y8P   '8'   888Y "
      , "  '8o          V          o8'  "]
      

wayne :: IO ()
wayne = putStrLn $ intercalate "\n" bat

parseWayne :: Parser (IO ())
parseWayne = subcommand "wayne" "Nanananananananananan" (pure wayne)

-- Bruce Lee parser
lee :: IO ()
lee = echo "Lee Jun-fan (Chinese: 李振藩; November 27, 1940 – July 20, 1973), known professionally as Bruce Lee (Chinese: 李小龍), was a Hong Kong American[3] martial artist, martial arts instructor, actor, director, and philosopher.[4] He was the founder of Jeet Kune Do, a hybrid martial arts philosophy drawing from different combat disciplines that is often credited with paving the way for modern mixed martial arts (MMA). Lee is considered by commentators, critics, media, and other martial artists to be the most influential martial artist of all time and a pop culture icon of the 20th century, who bridged the gap between East and West. He is credited with helping to change the way Asians were presented in American films.[5] "

parseLee :: Parser (IO ())
parseLee = subcommand "lee" "Facts about Bruce Lee" (pure lee)

-- REP loop for interactive

{-
eval :: String -> InputT IO ()
eval input = outputStr $ case parseString input of
               [Assign v e a] -> show e ++ "\n"
               [Express    b] -> ""
               [Import     f] -> "--- import successful\n"
               []             -> ""
               _              -> "--- too many lines\n"

repl :: IO ()
repl = runInputT defaultSettings (loop [])
   where 
       loop :: [String] -> InputT IO ()
       loop env = do
           minput <- getInputLine "λ> "
           case minput of
               Nothing            ->  return ()
               Just ":quit"       ->  return ()
               Just ":help"       ->  outputStrLn helpList *> loop env
               Just ":import"     ->  outputStrLn "--- import successful" *> loop env
--               Just m@('{':'+':s) -> (outputStrLn . show . parseMacro $ m)*> loop env
               Just (':':s)       ->  outputStrLn ("--- unkown command: " ++ s) *> loop env
               Just input         ->  catch (eval input)
                                      (\e -> outputStrLn (show (e :: SomeException))) 
                                      *> loop []


-- dont @ me, i know this is a stupid solution
helpList :: String -}
helpList = "Available commands:\n:quit, quits the repl\n:import, shorthand for '{! import file !}' in BruSKI"
 
type RState = Sequence
type Repl a = HaskelineT (StateT RState IO) a

instance MonadException (StateT RState IO) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in (`runStateT` s) <$> f run'

printr :: MonadIO m => String -> m ()
printr = liftIO . putStrLn

removeAss :: String -> Sequence -> Sequence
removeAss _ [] = []
removeAss n (Assign s e a : ss)
  | s == n    =                removeAss n ss
  | otherwise = Assign s e a : removeAss n ss
removeAss n (e : ss) = e     : removeAss n ss

getAss :: String -> Sequence -> Maybe Stmt
getAss n [] = Nothing
getAss n (Assign s e a : ss)
  | s == n    = Just $ Assign s e a
  | otherwise = getAss n ss
getAss n (e : ss) = getAss n ss

getBλ :: String -> Sequence -> Maybe Bλ
getBλ n ss = case getAss n ss of
                Just (Assign n2 e _) -> Just e
                _                    -> Nothing

prependTable s@[Assign n _ _] = state $ \ss -> ((), removeAss n ss ++ s)
prependTable s = state $ \ss -> ((), ss ++ s) 

runWithCurrent :: Sequence -> StateT RState IO ()
runWithCurrent exp = do
        curr <- get
        prog <- liftIO $ evalStateT (generateTable (curr ++ exp)) []
        var  <- liftIO $ run prog
        printr . show $ var

eval :: String -> Repl ()
eval input = case parseString input of
               as@[Assign {}] -> prependTable as
               ex@[Express _] -> lift (runWithCurrent ex)
               im@[Import  f] -> prependTable im
               []             -> return ()
               l              -> lift (runWithCurrent $ foldl1 App l)

ini :: Repl ()
ini = liftIO Sexy.welcome

fin :: Repl ()
fin = printr "Leaving bruci."

helpCmd [] = printr $ "Available commands:\n" ++ intercalate "\n" [fst x | x <- ops] ++ "\n\nFor info on a specific command, type :help COMMAND (or :? COMMAND)"
helpCmd ["help"  ] = printr "Usage: help [COMMAND]\nshow available commands, or info about certain commands"
helpCmd ["?"     ] = printr "Usage: ? [COMMAND]\nshow available commands, or info about certain commands"
helpCmd ["quit"  ] = printr "Usage: quit\nquits the repl"
helpCmd ["import"] = printr "Usage: import FILENAME\nshorthand for '{! import FILENAME !}'"
helpCmd ["format"] = printr "Usage: format VARNAME\nshorthand for '{! format VARNAME !}'"
helpCmd ["env"   ] = printr "Usage: env\nshows the current environment"
helpCmd ["clear" ] = printr "Usage: clear\nclear the current environment"
helpCmd ["delete"] = printr "Usage: delete VARNAME\ndeletes a statement from the environment"
helpCmd ["info"  ] = printr "Usage: info VARNAME\nshow the definition of VARNAME"
helpCmd ["int"   ] = printr "Usage: int VARNAME\nshow the value of VARNAME, as an int"
helpCmd ["chr"   ] = printr "Usage: chr VARNAME\nshow the value of VARNAME, as a char"
helpCmd ["sexy"  ] = printr "Usage: sexy\ndisplays the terminal greeting"
helpCmd [x       ] = printr $ "No such command :" ++ x
helpCmd  _         = printr "--- invalid arguments"

quitCmd    _   = abort
importCmd [f] = eval $ "{! import " ++ f ++ "!}"
importCmd  _  = printr "--- one import at a time, please"
formatCmd  λ  = eval $ "{! format " ++ concat λ ++ "!}"
envCmd     _  = get >>= printr . show
clearCmd   _  = state $ const ((),[])
deleteCmd [n] = state $ \ss -> ((), removeAss n ss)  
deleteCmd  _  = printr "--- one delete at a time, damn..."
infoCmd   [n] = do
        curr <- get
        case getBλ n curr of
          Just e  -> (printr . show) e
          Nothing -> printr $ "--- no function named " ++ n
infoCmd    _  = printr "--- Invalid arguments"
intCmd    [n] = do 
        curr <- get
        case getBλ n curr of
          Just s  -> (printr . show . decode) s
          Nothing -> printr $ "--- no function named " ++ n
intCmd     _  = printr "--- invalid arguments"
chrCmd    [n] = do
        curr <- get
        case getBλ n curr of
          Just s  -> (printr . show . chr . decode) s
          Nothing -> printr $ "--- no function named " ++ n
chrCmd     _  = printr "--- invalid arguments"
sexyCmd    _  = liftIO Sexy.welcome

ops :: [(String, [String] -> Repl ())]
ops = [ ("help"  , helpCmd  )
      , ("?"     , helpCmd  )
      , ("quit"  , quitCmd  )
      , ("import", importCmd)
      , ("format", formatCmd)
      , ("env"   , envCmd   )
      , ("clear" , clearCmd )
      , ("delete", deleteCmd)
      , ("info"  , infoCmd  )
      , ("int"   , intCmd   )
      , ("chr"   , chrCmd   )
      , ("sexy"  , sexyCmd  )
      ]

compl :: (Monad m, MonadState RState m) => WordCompleter m
compl n = do
  ss <- get
  return $ filter (isPrefixOf n) [n | (Assign n _ _ ) <- ss]

repl :: IO ()
repl = flip evalStateT [] $ 
        evalRepl (pure "λ> ") eval' ops (Just ':') (Word compl) ini
          where eval' input  = catch (eval input) (\e -> printr (show (e :: SomeException)))

replParser :: Parser (IO ())
replParser = subcommand "repl" "Interactive BruSKI prompt" (pure repl)

---- put it all together

parser :: Parser (IO ())
parser =  parseMain 
      <|> compileParser
      <|> replParser
      <|> runParser
      <|> genParser
      <|> parseVersion
--    <|> parseWayne
--    <|> parseLee

desc :: Description
desc = "\n                                bruc\n                         BruSKI -> Unlambda\n                       Version 0.4 - June 2020\n                           by Nicklas Botö"

main :: IO ()
main = join (Turtle.options desc parser)

