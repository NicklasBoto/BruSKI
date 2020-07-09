{-# LANGUAGE FlexibleInstances #-}

module AST
        ( Stmt (Assign, Express, Import)
        , Sequence, Unlambda, Symbol, SymbolTable
        , Bλ (Idx, Abs, App, Unl, Fun)
        , Iλ (Idx1, Abs1, App1, S1, K1, I1, Unl1)
        ) where

---- Format Import
import Data.List

-- Stores BruSKI epxressions
data Stmt = Assign String Bλ Integer 
          | Express Bλ
          | Import String

-- Stores DebBruijn expressions
data Bλ = Idx Integer
        | Abs Bλ
        | App Bλ Bλ
        | Unl String
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
        deriving (Show, Eq)

-- App Enc (Abs (Idx 0)) == Abs (App Enc (Idx 0))
-- Check β-equivalence (Remove abstractions, compute application) for Eq
-- Could just check SKI variant for equality

type Sequence    = [Stmt]
type Unlambda    = String
type Symbol      = (String, (Bλ, Int))
type SymbolTable = [Symbol]

instance Show Stmt where
        show (Assign s b a) = "[" ++ s ++ " , " ++ show b ++ ", " ++ show a ++ "]"
        show (Express    b) = "[!!, " ++ show b ++ "]"
        show (Import     s) = "[-> "  ++ show s ++ "]"

instance {-# OVERLAPPING #-} Show Sequence where
        show = intercalate "\n" . map show

instance {-# OVERLAPPING #-} Show SymbolTable where
        show = intercalate "\n" . map show

instance Show Bλ where
        show (Idx x)            = show x
        show (Abs s)            = "λ " ++ show s
        show (App l r)          = show l ++ " (" ++ show r ++ ")"
        show (Unl s)            = "{" ++ s ++ "}"
        show (Fun s a)          = show s ++ show a

toChar :: Integer -> Char
toChar = toEnum . fromInteger

