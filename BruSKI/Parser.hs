module Parser
        (
        ) where

---- IO Import
import System.IO

---- Monad Import
import Control.Monad

---- Parsec Import
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

---- Language Imports
import Expression.Parser
import Expression.DeBruijn
import Unlambda.Run
import Unlambda.AST

{-
Expressions: (Already parsed in Expression.Parser)

Bλ ::= Idx n | Unl s | Enc | Abs Bλ | App Bλ Bλ
SS ::= UNL x | INT x | CHR x

Statements: (Parsed here)

S ::= x := Bλ | anything else?
-}

data Stmt = Assign String Bλ 
          | Express Bλ
          | Sequent [Stmt]
          | Skip
          deriving Show

languageDef =
   emptyDef { Token.commentStart    = ";*"
            , Token.commentEnd      = "*;"
            , Token.commentLine     = ";"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "UNL"
                                      , "INT"
                                      , "CHR"
                                      , "skip"
                                      ]
            , Token.reservedOpNames = ["=:"]
            , Token.caseSensitive   = True 
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
colon      = Token.colon      lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> sequenceOfStmt

sequenceOfStmt = do
        list <- (sepEndBy statement (string "\n"))
        return (Sequent list)

statement :: Parser Stmt
statement = assignStmt <|> skipStmt

assignStmt :: Parser Stmt
assignStmt = do
        var <- identifier
        reservedOp ":="
        expr <- tryBλ
        return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

parseString :: String -> Stmt
parseString str = case parse whileParser "String Parser" str of
                    Left  e -> error $ show e
                    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
        program <- readFile file
        case parse whileParser "File Parser" program of
          Left  e -> print e >> fail "Parse Error in: "
          Right r -> return r

