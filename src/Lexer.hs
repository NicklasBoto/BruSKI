module Lexer
        ( identifier
        , reservedOp
        , whiteSpace
        , parens
        , braces
        , angles
        , natural
        , comma
        ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

---- Character Definitions
greek    = oneOf "αβδεφγηιθκμνοπχρστυξψζΑΒΔΕΦΓΗΙΘΚΛΜΝΟΠΧΡΣΤΥΞΨΖ"
math     = oneOf "∧∨⇔↔⇒→⊕⊻⩒¬←∀⋀∃⋁⩒∄⊢⊨⊤⊥∴∵∇∆∫∮≤≥≠±∓ℵℶ𝔠ℕℤℚℝℂ⊂⊆∈∉∅+*^"
allChars = alphaNum <|> greek <|> math

---- Lexer Definition
languageDef =
   emptyDef { Token.commentStart    = "{-"
            , Token.commentEnd      = "-}"
            , Token.commentLine     = "--"
            , Token.identStart      = allChars
            , Token.identLetter     = allChars
            , Token.reservedNames   = ["UNL" ,"INT", "CHR", "PRT"]
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
angles     = Token.angles     lexer
natural    = Token.natural    lexer
comma      = Token.comma      lexer

