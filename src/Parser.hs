module Parser
        ( parseString
        , parseFile
        , parseExpression
        ) where

{-
It always makes me so happy,
when you find someone alive.
    - Alaska State Troopers S4 E16
-}

---- Format Import
import Data.Char
import Main.Utf8 (withUtf8)

---- Parsec Import
import Text.ParserCombinators.Parsec

---- Language Import
import Encoding
import Lexer
import AST

---- Helpers

-- there probably is a function that does this... right?
wrapSpace func = do
        whiteSpace
        f <- func
        whiteSpace
        return f

---- Statement Parser
whileParser :: Parser Sequence
whileParser = do
        ld <- whileIDSL <|> return []
        s <- manyTill sequentStatement eof
        return $ ld ++ s

sequentStatement :: Parser Stmt
sequentStatement = wrapSpace statement

statement :: Parser Stmt
statement =  expressStmt
         <|> assignStmt

expressStmt :: Parser Stmt
expressStmt = reservedOp "!!" *> (Express <$> expression)

assignStmt :: Parser Stmt
assignStmt = do
        var <- identifier
        reservedOp ":="
        expr <- expression
        spaces
        arity <- getArity expr <|> countArity expr
        return $ Assign var expr arity

countArity :: Bλ -> Parser Integer
countArity = return . countLambda

getArity :: Bλ -> Parser Integer
getArity expr = do
    fullArity <- countArity expr
    reservedOp "::"
    arity <- natural

    if arity > fullArity then
        error "Parse Error\narity cannot be higher than the number of binders"
    else
        return arity

---- Isolated DSL parser (IDSL)
whileIDSL :: Parser Sequence
whileIDSL =  whileLangDef
         <|> whileInfo

---- Language Definiton Statement Parser
whileLangDef :: Parser Sequence
whileLangDef = string "{!" *> manyTill langdefStmt (string "!}")

langdefStmt :: Parser Stmt
langdefStmt = wrapSpace sequentLangDef

sequentLangDef :: Parser Stmt
sequentLangDef =  importLangDef
              <|> defineLangDef

importLangDef :: Parser Stmt
importLangDef = do
        string "import"
        spaces
        file <- many1 alphaNum
        return $ Import file

defineLangDef :: Parser Stmt
defineLangDef = do
        string "define"
        spaces
        name <- many1 alphaNum
        spaces
        value <- natural
        return $ Assign name (encode EncZ fromIntegral value) 0

---- Info Parser
whileInfo :: Parser Sequence
whileInfo = string "{?" *> manyTill infoStmt (string "?}")

infoStmt :: Parser Stmt
infoStmt = wrapSpace sequentInfo

sequentInfo :: Parser Stmt
sequentInfo =  authorInfo

authorInfo :: Parser Stmt
authorInfo = do
        string "author"
        spaces
        author <- many1 alphaNum
        return $ Assign author (encode EncZ fromIntegral 1) 0

---- Compiler Specific Expression Parser (CSEP)
forCompiler :: Parser Bλ
forCompiler =  funcExpression
           <|> skidExpression

funcExpression :: Parser Bλ
funcExpression = do
        name <- many1 alphaNum
        spaces
        args <- braces (sepBy (wrapSpace expression) comma) <|> return []
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
          <|> parens appExpression
          <|> synSugar
          <|> forCompiler

idxExpression :: Parser Bλ
idxExpression = Idx <$> natural

absExpression :: Parser Bλ
absExpression = reservedOp "λ" *> (Abs <$> expression)

appExpression :: Parser Bλ
appExpression = foldl1 App <$> sepBy1 expression spaces

synSugar :: Parser Bλ
synSugar =  unlP <|> prtP <|> intP <|> chrP

unlP, intP, chrP :: Parser Bλ
unlP = try $ string "UNL" *> (Unl <$> braces (many1 (noneOf "}")))
prtP = try $ string "PRT" *> (toPrint <$> braces (many1 (noneOf "}")))
intP = try $ string "INT" *> (encode EncZ toInt <$> braces (many1 digit))
chrP = try $ string "CHR" *> (encode EncX ord <$> braces anyChar)


---- User Input, Debug
parseString :: String -> Sequence
parseString str = case parse whileParser "String Parser" str of
                    Left  e -> error $ show e
                    Right r -> r

parseFile :: FilePath -> IO Sequence
parseFile file = withUtf8 $ do
        program <- readFile file
        case parse whileParser file program of
          Left  e -> print e >> fail "Parse Error"
          Right r -> return r

-- This is really only for debugging and testing purposes
parseExpression :: String -> Bλ
parseExpression str = case parse expression "Expression Parser" str of
                    Left  e -> error $ show e
                    Right r -> r
