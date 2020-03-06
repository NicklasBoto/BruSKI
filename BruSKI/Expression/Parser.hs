module Expression.Parser
        ( parseExpression
        , tryBλ
        ) where

---- Text Import
import Data.Char
import Text.Read

---- Parsing Import
import Text.Parsec

---- AST Import
import Expression.DeBruijn

parseExpression :: String -> Bλ
parseExpression = handleBλ . parse tryBλ "parseExpression"

handleBλ :: Either ParseError Bλ -> Bλ
handleBλ = either (error . ("\nParse Error in " ++) . show) id 

tryBλ =  try (space         *> tryBλ)
     <|> try (string "UNL{" *> (Unl <$> manyTill anyChar (try (char '}'))))
     <|> try (string "INT{" *> (encode toInt <$> manyTill digit (char '}')))
     <|> try (string "CHR{" *> (encode ord   <$> anyChar))
     <|> try (char ')'      *> tryBλ)
     <|> try (char '('      *> (App <$> tryBλ <*> tryBλ))
     <|> try (char 'λ'      *> (Abs <$> tryBλ))
     <|> try (indexRead <$> many1 digit)


encode :: (a -> Int) -> a -> Bλ
encode f c = Abs $ iterate (App Enc) (Idx 0) !! (f c)

decode :: Bλ -> Int
decode (Idx 0)     = 0
decode (App Enc b) = 1 + decode b
decode (Abs b)     = decode b 

toInt :: String -> Int
toInt = read

indexRead :: String -> Bλ
indexRead s = case readEither s of
                Left  e -> error $ "Index Parse Error (Probably a non-integer index)\n" ++ show e
                Right r -> Idx r

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

