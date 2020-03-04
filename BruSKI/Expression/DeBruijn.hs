module DeBruijn
        ( Bλ (Idx, Abs, App, Unl, Enc)
        ) where

data Bλ = Idx Int
        | Abs Bλ
        | App Bλ Bλ
        | Unl String
        | Enc
        deriving Eq

-- App Enc (Abs (Idx 0)) == Abs (App Enc (Idx 0))
-- Check β-equivalence (Remove abstractions, compute application) for Eq

instance Show Bλ where
        show (Idx x)   = show x
        show (Abs s)   = "λ " ++ show s
        show (App l r) = show l ++ " (" ++ show r ++ ")"
        show (Unl s)   = "{" ++ s ++ "}"
        show (Enc)     = "ζ"
