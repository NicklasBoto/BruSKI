module Parser
        ( parseString
        , parseFile
        , parseExpression
        , wrapSpace
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
import Config
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
        whiteSpace
        ld <- whileIDSL <|> return [] -- FIXME no mult. IDSLs 
        s <- manyTill sequentStatement eof
        return $ ld ++ s

sequentStatement :: Parser Stmt
sequentStatement = wrapSpace statement

statement :: Parser Stmt
statement =  try assignStmt
         <|> expressStmt

expressStmt :: Parser Stmt
expressStmt = Express <$> expression

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
 
    if (arity > fullArity) && Config.arityBlock then
        error "Parse Error\narity cannot be higher than the number of binders"
    else
        return arity

---- Isolated DSL parser (IDSL)
whileIDSL :: Parser Sequence
whileIDSL =  whileLangDef

---- Language Definiton Statement Parser
whileLangDef :: Parser Sequence
whileLangDef = string "{!" *> manyTill langdefStmt (string "!}")

langdefStmt :: Parser Stmt
langdefStmt = wrapSpace sequentLangDef

sequentLangDef :: Parser Stmt
sequentLangDef =  importLangDef
              <|> formatLangDef

importLangDef :: Parser Stmt
importLangDef = do
        string "import"
        spaces
        file <- many1 alphaNum
        return $ Import file

formatLangDef :: Parser Stmt
formatLangDef = do
        string "format"
        spaces
        formatter <- many1 (alphaNum <|> oneOf "}{)(")
        return $ Assign "__FORMATTER__" (parseExpression formatter) 1

funcExpression :: Parser Bλ
funcExpression = do
        name <- identifier
        spaces
        args <- braces (sepBy (wrapSpace expression) comma) <|> return []
        return $ Fun name args

---- Expression Parser
expression :: Parser Bλ
expression =  idxExpression
--        <|> try absCommented
          <|> absExpression
          <|> parens appExpression
          <|> synSugar
          <|> funcExpression
          <|> listParser

idxExpression :: Parser Bλ
idxExpression = Idx <$> natural

absExpression :: Parser Bλ
absExpression = reservedOp "λ" *> (Abs <$> expression)

-- FIXME parses weird
absCommented :: Parser Bλ
absCommented = do
        reservedOp "λ"
        manyTill letter (notFollowedBy (oneOf "λ."))
        char '.';
        whiteSpace
        Abs <$> expression

appExpression :: Parser Bλ
appExpression = foldl1 App <$> sepBy1 expression spaces

synSugar :: Parser Bλ
synSugar =  unlP
        <|> prtP
        <|> intP
        <|> chrP

unlP, intP, chrP :: Parser Bλ
unlP = try $ string "UNL" *> (Unl          <$> braces (many1 (noneOf "}")))
prtP = try $ string "PRT" *> (toPrint      <$> braces (many1 (noneOf "}")))
intP = try $ string "INT" *> (encode toInt <$> braces (many1 digit))
chrP = try $ string "CHR" *> (encode ord   <$> braces anyChar)

listParser :: Parser Bλ
listParser =  try unlL 
          <|> try intL
          <|> chrL
          <|> listIndex
          <|> listP
          <|> pairP

unlL :: Parser Bλ
unlL = do
        string "UNL"
        unls <- brackets (sepBy (many1 $ noneOf "],") comma)
        let mapU = map Unl unls
        return $ toList mapU

intL :: Parser Bλ
intL = do
        string "INT"
        ints <- brackets (sepBy1 (many1 digit) comma)
        let encI = map (encode toInt) ints
        return $ toList encI

chrL :: Parser Bλ
chrL = do
        char '"'
        chrs <- manyTill anyChar (try (string "\""))
        let encC = map (encode ord) chrs
        return $ toList encC

listIndex :: Parser Bλ
listIndex = do
        name <- identifier
        idx  <- brackets (many1 digit)
        return $ let eI = encode toInt idx 
                 in  Fun "get" [eI, Fun name []]

listP :: Parser Bλ
listP = toList <$> brackets (sepBy (wrapSpace expression) comma)

pairP :: Parser Bλ
pairP = toPair <$> angles (sepBy expression comma)
        where toPair :: [Bλ] -> Bλ
              toPair [a, b] = Fun "pair" [a, b]
              toPair _      = error "Parse Error\ntoo many elements"

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
