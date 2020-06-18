{-# LANGUAGE FlexibleInstances #-}

module AST
        ( Stmt (Assign, Express, Import)
        , Sequence
        , Bλ (Idx, Abs, App, Unl, EncZ, EncX, Prc, Fun)
        , Iλ (Idx1, Abs1, App1, S1, K1, I1, Unl1, D1)
        ) where

---- Format Import
import Data.List

-- Stores BruSKI epxressions
data Stmt = Assign String Bλ Integer 
          | Express Bλ
          | Import String

type Sequence = [Stmt]

-- Stores DebBruijn expressions
data Bλ = Idx Integer
        | Abs Bλ
        | App Bλ Bλ
        | Unl String
        | EncZ -- From here on down are compiler specific values
        | EncX
        | Prc Integer
        | Fun String [Bλ]
        deriving Eq

-- Stores intermediate expressions
data Iλ = Idx1 Int
        | Abs1 Iλ
        | App1 Iλ Iλ
        | S1
        | K1
        | I1
        | Unl1 String
        | D1 String
        deriving (Show, Eq)

-- App Enc (Abs (Idx 0)) == Abs (App Enc (Idx 0))
-- Check β-equivalence (Remove abstractions, compute application) for Eq
-- Could just check SKI variant for equality

{- Because windows terminal is mad at unicode
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
-}

instance Show Stmt where
        show (Assign s b a) = "[" ++ s ++ " , " ++ show b ++ ", " ++ show a ++ "]"
        show (Express    b) = "[!!, " ++ show b ++ "]"
        show (Import     s) = "[-> " ++ show s ++ "]"

instance {-# OVERLAPPING #-} Show Sequence where
    show = intercalate "\n" . map show

instance Show Bλ where
        show (Idx x)            = show x
        show (Abs (App EncZ r)) = "z(" ++  show   (1 + decode r) ++ ")"
        show (Abs (App EncX r)) = "x(" ++ [toChar (1 + decode r)] ++ ")"
        show (Abs s)            = "l " ++ show s
        show (App l r)          = show l ++ " (" ++ show r ++ ")"
        show (Unl s)            = "{" ++ s ++ "}"
        show (EncZ)             = "z"
        show (EncX)             = "x"
        show (Prc x)            = "%" ++ show x
        show (Fun s a)          = show s ++ show a

decode :: Bλ -> Integer
decode (Idx 0)      = 0
decode (App EncZ b) = 1 + decode b
decode (App EncX b) = 1 + decode b
decode (Abs b)      = decode b

toChar :: Integer -> Char
toChar = toEnum . fromInteger
