{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Generator
import Turtle
import Paths_BruSKI (version)
import Data.Version (showVersion)
import Control.Monad
import Data.List
import System.IO
import System.Directory
import qualified Sexy as Sexy

-- main parser
mainSubroutine :: IO ()
mainSubroutine = Sexy.welcome

parseMain :: Parser (IO ())
parseMain = pure mainSubroutine

-- filepath parser
getOutputPath :: Parser (Turtle.FilePath, Maybe Turtle.FilePath)
getOutputPath = (,) <$> (argPath "source" "Input file of compiler")
                    <*>  optional (optPath "target" 'o' "Output file of compiler")

compileToFile :: Turtle.FilePath -> Maybe Turtle.FilePath -> IO ()
compileToFile source Nothing = writeToFile (encodeString source) (generateFileName ".")
compileToFile source (Just target)
  | filename target == emptyPath = writeToFile fSource (generateFileName fTarget)
  | otherwise                    = writeToFile fSource                   fTarget
    where fSource   = encodeString source
          fTarget   = encodeString target
          emptyPath = decodeString ""

writeToFile :: System.IO.FilePath -> System.IO.FilePath -> IO ()
writeToFile source target = do
        file <- openFile target WriteMode 
        compiled <- liftM fst . genFile $ source
        hPutStrLn file compiled 
        hClose file

-- generateFileName :: System.IO.FilePath -> String
generateFileName folder = "poop.ski"

compileParser :: Parser (IO ())
compileParser = fmap (uncurry compileToFile) (subcommand "compile" "Compile BruSKI source code to Unlambda file" getOutputPath)

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

-- Bruce Wayne parser
bat = [ "       _,    _   _    ,_       "
      , "  .o888P     Y8o8Y     Y888o.  "
      , " d88888      88888      88888b "
      , "d888888b_  _d88888b_  _d888888b"
      , "8888888888888888888888888888888"
      , "8888888888888888888888888888888"
      , "YJGS8PYY888PYY888PYY888PYY8888P"
      , " Y888   '8'   Y8P   '8'   888Y "
      , "  '8o          V          o8'  "
      ]

wayne :: IO ()
wayne = putStrLn $ intercalate "\n" bat

parseWayne :: Parser (IO ())
parseWayne = (subcommand "wayne" "Nanananananananananan" (pure wayne))

-- Bruce Lee parser
lee :: IO ()
lee = echo "Lee Jun-fan (Chinese: 李振藩; November 27, 1940 – July 20, 1973), known professionally as Bruce Lee (Chinese: 李小龍), was a Hong Kong American[3] martial artist, martial arts instructor, actor, director, and philosopher.[4] He was the founder of Jeet Kune Do, a hybrid martial arts philosophy drawing from different combat disciplines that is often credited with paving the way for modern mixed martial arts (MMA). Lee is considered by commentators, critics, media, and other martial artists to be the most influential martial artist of all time and a pop culture icon of the 20th century, who bridged the gap between East and West. He is credited with helping to change the way Asians were presented in American films.[5] "

parseLee :: Parser (IO ())
parseLee = (subcommand "lee" "Facts about Bruce Lee" (pure lee))

parser :: Parser (IO ())
parser =  parseMain 
      <|> parseVersion
      <|> parseLee
      <|> compileParser
      <|> parseWayne

desc :: Description
desc = "\n                                bruc\n                         BruSKI -> Unlambda\n                       Version 0.4 - June 2020\n                           by Nicklas Botö"

main :: IO ()
main = do
        cmd <- options desc parser
        cmd
