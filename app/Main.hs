{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Generator
import Turtle
import Paths_BruSKI (version)
import Data.Version (showVersion)
import Control.Monad
import System.IO
import qualified Sexy as Sexy

-- main parser
mainSubroutine :: IO ()
mainSubroutine = echo "Hello BruSKI!"

parseMain :: Parser (IO ())
parseMain = pure mainSubroutine

-- compilerVersion :: IO ()
-- compilerVersion = putStrLn (showVersion version)
-- 
-- verboseCompilerVersion :: IO ()
-- verboseCompilerVersion = do
--         compilerVersion
--         -- echo "This compiler is currently in development!\nPlease notify me at [mail@nicklasbotö.se], or open an issue at [bruski.nicklasbotö.se] if you want a certain feature."
--                                                                                                                                parseVersion :: Parser (IO ())
-- parseVersion = (subcommand "version" "Show current compiler version" (pure verboseCompilerVersion))

-- filepath parser
getOutputPath :: Parser (Turtle.FilePath, Maybe Turtle.FilePath)
getOutputPath = (,) <$>  (argPath "source" "Input file of compiler")
                    <*>   optional (optPath "target" 'o' "Output file of compiler")

compileToFile :: Turtle.FilePath -> Maybe Turtle.FilePath -> IO ()
compileToFile source Nothing       = error $ "Generator Error\nplease specify output file name\nexample: bruc source -o target"
compileToFile source (Just target) = do
        file <- openFile (encodeString target) WriteMode 
        compiled <- liftM fst . genFile $ encodeString source
        hPutStrLn file compiled 
        hClose file

compileParser :: Parser (IO ())
compileParser = fmap (uncurry compileToFile) (subcommand "compile" "compile BruSKI source code to Unlambda file" getOutputPath)

-- version parser
version' :: IO()
version' = putStrLn (showVersion version)

verboseVersion :: IO()
verboseVersion = do
                 version'
                 echo "This compiler is currently in development! Please notify me at [mail@nicklasbotö.se], or open an issue at [bruski.nicklasbotö.se] if you want a certain feature."

parseVersion :: Parser (IO ())
parseVersion =
    (subcommand "version" "Show compiler version" (pure verboseVersion))

-- Bruce Lee parser
lee :: IO ()
lee = echo "Lee Jun-fan (Chinese: 李振藩; November 27, 1940 – July 20, 1973), known professionally as Bruce Lee (Chinese: 李小龍), was a Hong Kong American[3] martial artist, martial arts instructor, actor, director, and philosopher.[4] He was the founder of Jeet Kune Do, a hybrid martial arts philosophy drawing from different combat disciplines that is often credited with paving the way for modern mixed martial arts (MMA). Lee is considered by commentators, critics, media, and other martial artists to be the most influential martial artist of all time and a pop culture icon of the 20th century, who bridged the gap between East and West. He is credited with helping to change the way Asians were presented in American films.[5] "

parseLee :: Parser (IO ())
parseLee = (subcommand "lee" "Facts about Bruce Lee" (pure lee))

parser :: Parser (IO ())
parser = parseMain <|> parseVersion <|> parseLee <|> compileParser

desc :: Description
desc = "\n                                bruc\n                         BruSKI -> Unlambda\n                       Version 0.4 - March 2020\n                           by Nicklas Botö"

main :: IO ()
main = do
        cmd <- options desc parser
        cmd
