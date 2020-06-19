{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Generator
import Turtle
import Paths_BruSKI (version)
import Data.Version (showVersion)
import qualified Sexy as Sexy

mainSubroutine :: IO ()
mainSubroutine = echo "Hello BruSKI!"

-- compilerVersion :: IO ()
-- compilerVersion = putStrLn (showVersion version)
-- 
-- verboseCompilerVersion :: IO ()
-- verboseCompilerVersion = do
--         compilerVersion
--         -- echo "This compiler is currently in development!\nPlease notify me at [mail@nicklasbotö.se], or open an issue at [bruski.nicklasbotö.se] if you want a certain feature."
--                                                                                                                                parseVersion :: Parser (IO ())
-- parseVersion = (subcommand "version" "Show current compiler version" (pure verboseCompilerVersion))

-- version
version' :: IO()
version' = putStrLn (showVersion version)

verboseVersion :: IO()
verboseVersion = do
                 version'
                 echo "This compiler is currently in development! Please notify me at [mail@nicklasbotö.se], or open an issue at [bruski.nicklasbotö.se] if you want a certain feature."

parseVersion :: Parser (IO ())
parseVersion =
    (subcommand "version" "Show compiler version" (pure verboseVersion))

parseMain :: Parser (IO ())
parseMain = pure mainSubroutine

parser :: Parser (IO ())
parser = parseMain <|> parseVersion

desc :: Description
desc = "\n                                BruSKI\n                           DeBruijn -> SKI\n                       Version 0.4 - March 2020\n                           by Nicklas Botö"

main :: IO ()
main = do
        cmd <- options desc parser
        cmd
