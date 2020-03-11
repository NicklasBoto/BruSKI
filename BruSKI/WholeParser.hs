module Parser
        ( parseString
        , parseFile
        ) where

---- Show Import
import Data.List
import Data.Char

---- Parsec Import
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

---- Language Import
import Expression.Parser
import Expression.DeBruijn

{-
Expressions: (Already parsed in Expression.Parser)

Bλ       ::= Idx n | Unl s | Enc | Abs Bλ | App Bλ Bλ | Fun s Bλ | Prc n
SynSugar ::= UNL x | INT x | CHR x

Statements: (Parsed here)

S ::= x := Bλ :: n | !! Bλ
-}

brewBeer :: [String]
brewBeer = [ "                                                                                          "
           , "                                                                                          "
           , "                                                                                          "
           , "                                  ``````                                                  "
           , "                               `````````````                                              "
           , "                 `````````````````````````````     ``````                                 "
           , "               ``````````````````````````````````````````````                             "
           , "             ``````````````````````````````````````````````````                           "
           , "             ```````````````````````````````````````````````````                          "
           , "           ````````````````````````````````````````````````````````````                   "
           , "        `````````````````````````````````````````````````````````````````                 "
           , "       ``````````````````````````````````````````````````````````````````                 "
           , "       ``````````````````````````````````````````````````````````````````                 "
           , "       `````````````````````````````````````````````````````````````````                  "
           , "        `````````````````````````````````````````````````````````````                     "
           , "           ```````````````````````````````````````````````````````                        "
           , "             ```````````--```````````````````````````````````````                         "
           , "             .```````.-://::-.``````````.-:-``````````-````````                           "
           , "             ....-::://///////::::-::::////-`````````.//:-.````                           "
           , "             ....://///////////////////////-`````````.////-....`````````                  "
           , "             ....://///////////////////////-`````````.////-...............``              "
           , "             ....://////+++++//////+++++///-`````````.////-..................`            "
           , "             ....://///++++++//////++++++//:`````````.////-...................`           "
           , "             ....://///++oooo++////++++++///.````````:////-....```````........`           "
           , "             ....://///syssoo+osso/++++++////:--..-///////-....        `......`           "
           , "             ....:///+yoosyssso+/+ys+++++//////+++++//////-....         ......`           "
           , "             ....:///hoyso+++//oy+/sy++++//////+++++//////-....         `.....`           "
           , "             ....://+yyo+++++////ss+sy+++//////+++++//////-....         `.....`           "
           , "             ....://///++++++/////sy+sy++//////+++++//////-....         `.....`           "
           , "             ....://///++++++/////oy++ys+//////+++++//////-....         `.....`           "
           , "             ....://///++++++////oyo++oh+//////+++++//////-....         `.....`           "
           , "             ....://///++++++///os/++++sy//////+++++//////-....         `.....`           "
           , "             ....://///++++++/+////++ss+yo/////+++++//////-....         `.....`           "
           , "             ....://///++++++ys////oyshooh/////+++++//////-....         `.....`           "
           , "             ....://///++++oo/////syo+sy+oy////+++++//////-....         `.....`           "
           , "             ....://///++oys+///+yo++++ys/ss///+++++//////-....         `.....`           "
           , "             ....://///+syo++//ss+/+++++y+/ss//+++++//+++/-....         `.....`           "
           , "             ....:////+ys+++++yo///++++++y+/oso+++++/+yys/-....        `......`           "
           , "             ....:///oyo++++ss+////++++++/ss/+osssssso+y+/-....````````.......`           "
           , "             ....:/+ys++oooys//////++++++//oso+ooooo+//+//-...................`           "
           , "             ....:/osoosssso+//////++++++////+ossssso/////-..................`            "
           , "             ....://///++++++//////++++++//////+++++//////-................``             "
           , "             ....-//////+++++//////+++++///////+++++//////-....```````````                "
           , "             ....-////////////////////////////////////////-....                           "
           , "             .....://////////////////////////////////////:.....                           "
           , "             `.....-://////////////////////////////////:-.....`                           "
           , "              `......----------------------------------......`                            "
           , "               ```........................................```                             "]

brewBeerText :: String
brewBeerText = "\n                                BruSKI\n                           DeBruijn -> SKI\n                       Version 0.4 - March 2020\n                           by Nicklas Botö" 

welcome :: IO ()
welcome = putStrLn $ (intercalate "\n" $ brewBeer) ++ brewBeerText

---- Abstract Syntax Tree
data Stmt = Assign String Bλ Integer
          | Express Bλ
          | Sequent [Stmt]

instance Show Stmt where
        show (Assign s b a) = "[" ++ s ++ " , " ++ show b ++ ", " ++ show a ++ "]"
        show (Express  b) = "[!!, " ++ show b ++ "]"
        show (Sequent s)  = intercalate "\n" $ map show s


---- Lexer Definition
languageDef =
   emptyDef { Token.commentStart    = ";*"
            , Token.commentEnd      = "*;"
            , Token.commentLine     = ";"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["UNL" ,"INT", "CHR", "BOL"]
            , Token.reservedOpNames = [":=", "!!", "::", "λ", "%"]
            , Token.caseSensitive   = True 
            }

-- This saved my life
lexer = Token.makeTokenParser languageDef

identifier, comma :: Parser String
parens, braces    :: Parser a -> Parser a
reservedOp        :: String -> Parser ()
whiteSpace        :: Parser ()
natural           :: Parser Integer     

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
natural    = Token.natural    lexer
comma      = Token.comma      lexer


---- Statement Parser
whileParser :: Parser Stmt
whileParser = Sequent <$> manyTill sequentStatement eof

sequentStatement :: Parser Stmt
sequentStatement = whiteSpace *> statement

statement :: Parser Stmt
statement =  expressStmt <|> assignStmt 

expressStmt :: Parser Stmt
expressStmt = reservedOp "!!" *> (Express <$> expression)

assignStmt :: Parser Stmt
assignStmt = do
        var <- identifier
        reservedOp ":="
        expr <- expression
        spaces
        arity <- getArity <|> countArity expr
        return $ Assign var expr arity

countArity :: Bλ -> Parser Integer
countArity = return . countLambda

getArity :: Parser Integer
getArity = reservedOp "::" *> natural


---- Compiler Specific Expression Parser (CSEP)
forCompiler :: Parser Bλ
forCompiler =  funcExpression
           <|> skidExpression

funcExpression :: Parser Bλ
funcExpression = do
        name <- many1 alphaNum
        args <- braces (sepBy expression comma) <|> return []
        return $ Fun name args

skidExpression :: Parser Bλ
skidExpression = do
        reservedOp "%"
        index <- toInteg <$> many1 digit
        return $ Prc index


---- Expression Parser
expression :: Parser Bλ
expression =  idxExpression
          <|> absExpression
          <|> appExpression
          <|> synSugar
          <|> forCompiler

idxExpression :: Parser Bλ
idxExpression = Idx <$> natural

absExpression :: Parser Bλ
absExpression = reservedOp "λ" *> (Abs <$> expression)

appExpression :: Parser Bλ
appExpression = do
        l <- parens expression
        spaces
        r <- parens expression
        return $ App l r
 
synSugar :: Parser Bλ
synSugar =  unlP
        <|> intP
        <|> chrP

unlP, intP, chrP :: Parser Bλ
unlP = string "UNL" *> (Unl <$> braces (many1 (noneOf "}")))
intP = string "INT" *> (encode toInt <$> braces (many1 digit))
chrP = string "CHR" *> (encode ord <$> braces anyChar)


---- User Input, Debug
parseString :: String -> Stmt
parseString str = case parse whileParser "String Parser" str of
                    Left  e -> error $ show e
                    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
        program <- readFile file
        case parse whileParser file program of
          Left  e -> print e >> fail "Parse Error"
          Right r -> return r

