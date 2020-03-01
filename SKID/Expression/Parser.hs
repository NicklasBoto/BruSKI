module Expression.Parser
        ( parseExpression
        ) where

import Data.Char
import Text.Parsec
import Expression.DeBruijn

parseExpression :: String -> Bλ
parseExpression = handleBλ . parse tryBλ "parseExpression" . uncomment

handleBλ :: Either ParseError Bλ -> Bλ
handleBλ = either (error . ("\nParse Error in " ++) . show) id 

tryBλ =  try (space    *> tryBλ)
     <|> try (char ')' *> tryBλ)
     <|> try (char '(' *> (App <$> tryBλ <*> tryBλ))
     <|> try (char 'λ' *> (Abs <$> tryBλ))
     <|> try (string "UNL{" *> (Unl <$> manyTill anyChar (try (char '}'))))
     <|> try (Idx . (\x -> read x :: Int) <$> (manyTill digit (notFollowedBy digit)))

  -- There has got to be a better way to do this!

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

