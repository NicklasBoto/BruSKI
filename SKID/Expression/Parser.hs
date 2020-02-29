module Parser
        (
        ) where

import Data.Char
import Text.Parsec
import DeBruijn

parseExpression :: String -> Bλ
parseExpression = handleBλ . parse tryBλ "parseExpression" . uncomment

handleBλ :: Either ParseError Bλ -> Bλ
handleBλ = either (error . ("\nParse Error in " ++) . show) id 

-- Abs (Abs (Idx 1))
-- λ    (λ  (    1))
-- λλ 1

-- Abs (Abs (Abs (App (App (Idx 2) (Idx 0)) (App (Idx 1) (Idx 0)))))
-- λλλ (2 0) (1 0) 
--
-- SWISHA MASSA PENGAR

tryBλ =  try (space    *> tryBλ)
     <|> try (char ')' *> tryBλ)
     <|> try (char '(' *> (App <$> tryBλ <*> tryBλ))
     <|> try (char 'λ' *> (Abs <$> tryBλ))
     <|> try (Idx . (\x -> read x :: Int) <$> (manyTill digit (notFollowedBy digit)))
  -- <|> try (eof *> ???

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

