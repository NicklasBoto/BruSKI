module DeBruijn
        ( Bλ (Idx, Abs, App)
        ) where

data Bλ = Idx Int
        | Abs Bλ
        | App Bλ Bλ
        deriving Show

        {-
instance Show Bλ where
        show (Idx x)   = show x
        show (Abs s)   = "λ " ++ show s
        show (App l r) = "(" ++ show l ++ show r ++ ")"
        -}
