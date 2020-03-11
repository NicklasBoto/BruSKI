module AST
        ( Stmt (Assign, Express, Sequent)
        , Bλ (Idx, Abs, App, Unl, Enc, Prc, Fun)
        ) where

---- Format Import
import Data.List

data Stmt = Assign String Bλ Integer
          | Express Bλ
          | Sequent [Stmt]

data Bλ = Idx Integer
        | Abs Bλ
        | App Bλ Bλ
        | Unl String
        | Enc -- From here on down are compiler specific values
        | Prc Integer
        | Fun String [Bλ]
        deriving Eq

-- App Enc (Abs (Idx 0)) == Abs (App Enc (Idx 0))
-- Check β-equivalence (Remove abstractions, compute application) for Eq

instance Show Stmt where
        show (Assign s b a) = "[" ++ s ++ " , " ++ show b ++ ", " ++ show a ++ "]"
        show (Express  b) = "[!!, " ++ show b ++ "]"
        show (Sequent s)  = intercalate "\n" $ map show s


instance Show Bλ where
        show (Idx x)           = show x
        show (Abs (App Enc r)) = "ζ(" ++ show (1 + decode r) ++ ")"
        show (Abs s)           = "λ " ++ show s
        show (App l r)         = show l ++ " (" ++ show r ++ ")"
        show (Unl s)           = "{" ++ s ++ "}"
        show (Enc)             = "ζ"
        show (Prc x)           = "%" ++ show x
        show (Fun s a)         = show s ++ show a

decode :: Bλ -> Integer
decode (Idx 0)     = 0
decode (App Enc b) = 1 + decode b
decode (Abs b)     = decode b

