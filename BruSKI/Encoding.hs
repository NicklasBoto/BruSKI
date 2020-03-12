module Encoding
        ( encode
        , decode
        , countLambda
        , toInteg
        , toInt
        , toPrint
        ) where


---- Format Import
import Data.List

---- Language Import
import AST

toPrint :: String -> Bλ
toPrint s = Unl $ pr ++ '.' : intersperse '.' s ++ "i"
        where pr = replicate (length s) '`'

encode :: (a -> Int) -> a -> Bλ
encode f c = Abs $ iterate (App Enc) (Idx 0) !! (f c)

decode :: Bλ -> Integer
decode (Idx 0)     = 0
decode (App Enc b) = 1 + decode b
decode (Abs b)     = decode b

countLambda :: Bλ -> Integer
countLambda (App l r) = countLambda l + countLambda r
countLambda (Abs b)   = 1 + countLambda b
countLambda _         = 0

toInteg :: String -> Integer
toInteg = read

toInt :: String -> Int
toInt = read
