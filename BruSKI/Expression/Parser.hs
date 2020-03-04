module Parser
        ( parseExpression
        ) where

import Data.Char
import Text.Parsec
import DeBruijn

p = parseExpression

parseExpression :: String -> Bλ
parseExpression = handleBλ . parse tryBλ "parseExpression" . uncomment

handleBλ :: Either ParseError Bλ -> Bλ
handleBλ = either (error . ("\nParse Error in " ++) . show) id 

tryBλ =  try (space         *> tryBλ)
     <|> try (string "UNL{" *> (Unl <$> manyTill anyChar (try (char '}'))))
     <|> try (string "INT{" *> (encode toInt <$> manyTill digit (char '}')))
     <|> try (string "CHR{" *> (encodeChar <$> anyChar))
     <|> try (char ')'      *> tryBλ)
     <|> try (char '('      *> (App <$> tryBλ <*> tryBλ))
     <|> try (char 'λ'      *> (Abs <$> tryBλ))
     <|> try (Idx . toInt <$> manyTill digit (notFollowedBy digit))
                        -- ^                  ^^^^^^^^^^^^^^ Reduce this..
                        -- | move this to separate function, with error handling

encodeChurch :: String -> Bλ
encodeChurch = encode toInt

encodeChar :: Char -> Bλ
encodeChar = encode ord

encode :: (a -> Int) -> a -> Bλ
encode f c = Abs $ iterate (App Enc) (Idx 0) !! (f c)

decode :: Bλ -> Int
decode (Idx 0)     = 0
decode (App Enc b) = 1 + decode b
decode (Abs b)     = decode b 

toInt :: String -> Int
toInt = read

uncomment :: String -> String
uncomment []           = []
uncomment (';':'*':cs) = uncomment (dropUntil (/=';' ) cs)
uncomment (';':cs)     = uncomment (dropUntil (/='\n') cs)
uncomment (c  :cs)     = c : uncomment cs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] =  []
dropUntil p (x:xs)
            | p x = dropUntil p xs
            | otherwise =  xs

