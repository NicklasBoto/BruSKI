module Parser
        ( parseString
        , parseFile
        ) where

---- IO Import
import System.IO

---- Show Import
import Data.List
import Data.Char

---- Monad Import
import Control.Monad

---- Parsec Import
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

---- Language Import
import Expression.Parser
import Expression.DeBruijn
import Unlambda.Run
import Unlambda.AST

{-
Expressions: (Already parsed in Expression.Parser)

Bλ       ::= Idx n | Unl s | Enc | Abs Bλ | App Bλ Bλ | Fun s Bλ | Prc n
SynSugar ::= UNL x | INT x | CHR x

Statements: (Parsed here)

S ::= x := Bλ :: n | !! Bλ
-}


---- Abstract Syntax Tree
data Stmt = Assign String Bλ Int
          | Express Bλ
          | Sequent [Stmt]
          | Skip

instance Show Stmt where
        show (Assign s b a) = "[" ++ s ++ " , " ++ show b ++ ", " ++ show a ++ "]"
        show (Express  b) = "[!!, " ++ show b ++ "]"
        show (Skip)       = "=>"
        show (Sequent s)  = intercalate "\n" $ map show s


---- Lexer Definition
languageDef =
   emptyDef { Token.commentStart    = ";*"
            , Token.commentEnd      = "*;"
            , Token.commentLine     = ";"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["UNL","INT", "CHR", "SKIP"]
            , Token.reservedOpNames = [":=", "!!", "::", "λ", "%"]
            , Token.caseSensitive   = True 
            }

-- This saved my life
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
comma      = Token.comma      lexer


---- Statement Parser
whileParser :: Parser Stmt
whileParser = whiteSpace >> sequenceOfStmt

sequenceOfStmt = do
        list <- (sepEndBy statement (string "\n"))
        return (Sequent list)

statement :: Parser Stmt
statement =  expressStmt 
         <|> assignStmt 
         <|> skipStmt

expressStmt :: Parser Stmt
expressStmt = do
        reservedOp "!!"
        expr <- expression
        return $ Express expr

assignStmt :: Parser Stmt
assignStmt = do
        var <- identifier
        reservedOp ":="
        expr <- expression
        _ <- spaces
        arity <- getArity <|> countArity expr
        return $ Assign var expr arity

countArity :: Bλ -> Parser Int
countArity = return . countLambda

getArity :: Parser Int 
getArity = do
        reservedOp "::"
        _ <- spaces
        toInt <$> many1 digit

skipStmt :: Parser Stmt
skipStmt = reserved "SKIP" >> return Skip


---- Compiler Specific Expression Parser (CSEP)
forCompiler :: Parser Bλ
forCompiler =  funcExpression
           <|> skidExpression

funcExpression :: Parser Bλ
funcExpression = do
        name <- many1 alphaNum
        args <- braces (sepBy expression comma)
        return $ Fun name args

skidExpression :: Parser Bλ
skidExpression = do
        reservedOp "%"
        index <- toInt <$> many1 digit
        return $ Prc index


---- Expression Parser
expression :: Parser Bλ
expression =  idxExpression
          <|> absExpression
          <|> appExpression
          <|> synSugar
          <|> forCompiler

idxExpression :: Parser Bλ
idxExpression = indexRead <$> many1 digit

absExpression :: Parser Bλ
absExpression = do
        _    <- char 'λ'
        body <- expression
        return $ Abs body

appExpression :: Parser Bλ
appExpression = do
        l <- parens expression
        r <- parens expression
        return $ App l r
 
synSugar :: Parser Bλ
synSugar =  try (string "UNL{" *> (Unl <$> manyTill anyChar (try (char '}'))))
        <|> try (string "INT{" *> (encode toInt <$> manyTill digit (char '}')))
        <|> try (string "CHR{" *> (encode ord   <$> anyChar))


---- User Input, Debug
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

