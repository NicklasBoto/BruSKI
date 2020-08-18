module Encoding
        ( encode
        , decode
        , countLambda
        , toInteg
        , toInt
        , toPrint
        , toList
        , toChar
        ) where


---- Format Import
import Data.List

---- Handling Import
import Data.Maybe

---- Language Import
import AST

toPrint :: String -> Bλ
toPrint s = Unl $ pr ++ '.' : intersperse '.' s ++ "i"
        where pr = replicate (length s) '`'

toList :: [Bλ] -> Bλ
toList = foldr cons nil
        where cons a b = Fun "cons" [a, b]
              nil  = Fun "nil"  []

encode :: (a -> Int) -> a -> Bλ
encode f c = Abs . Abs $ iterate (App (Idx 1)) (Idx 0) !! f c

decode :: Bλ -> Maybe Int
decode (Abs (Abs    (Idx 0))) = Just 0
decode (App (Idx 1) (Idx 0))  = Just 1
decode (Abs              λ)   = decode λ
decode (App (Idx 1)      r )  = fmap (+1) (decode r)
decode                   _    = Nothing

countLambda :: Bλ -> Integer
countLambda (App l r) = countLambda l + countLambda r
countLambda (Abs b)   = 1 + countLambda b
countLambda _         = 0

toInteg :: String -> Integer
toInteg = read

toInt :: String -> Int
toInt = read

toChar :: Integer -> Char
toChar = toEnum . fromInteger
