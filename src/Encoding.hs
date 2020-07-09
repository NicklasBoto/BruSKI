module Encoding
        ( encode
        , countLambda
        , toInteg
        , toInt
        , toPrint
        , toChar
        ) where


---- Format Import
import Data.List

---- Language Import
import AST

toPrint :: String -> Bλ
toPrint s = Unl $ pr ++ '.' : intersperse '.' s ++ "i"
        where pr = replicate (length s) '`'

encode :: (a -> Int) -> a -> Bλ
encode f c = Abs . Abs $ iterate (App (Idx 1)) (Idx 0) !! f c

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
