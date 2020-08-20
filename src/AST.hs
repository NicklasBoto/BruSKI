{-# LANGUAGE FlexibleInstances #-}

module AST
        ( Stmt (Assign, Express, Import)
        , Sequence, Unlambda, Symbol, SymbolTable
        , Bλ (Idx, Abs, App, Unl, Fun)
        , Iλ (Idx1, Abs1, App1, S1, K1, I1, Unl1)
        ) where

---- Format Import
import Data.List
import Data.Maybe

-- Stores BruSKI epxressions
data Stmt = Assign String Bλ Integer 
          | Express Bλ
          | Import String

-- Stores DebBruijn expressions
data Bλ = Idx Integer
        | Abs Bλ
        | App Bool Bλ Bλ
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
        show (Idx     x) = show x
        show (Abs     s) = case decode (Abs s) of
                           -- Because INT{0} is λλ0 which is used
                           -- all the time, also 1 why not.
                           Just i  -> if   i < 2 
                                      then "λ" ++ show s
                                      else "INT{" ++ show i ++ "}"
                           Nothing -> "λ" ++ show s
        show (App _ l r) = "(" ++ show l ++ "<-" ++ show r ++ ")"
        show (Unl     s) = "{" ++ s ++ "}"
        show (Fun s   a) = show s ++ show a

toChar :: Integer -> Char
toChar = toEnum . fromInteger

-- Makes showing the environment MUCH better
-- not cluttered with numerals
decode :: Bλ -> Maybe Int
decode (Abs (Abs      (Idx 0))) = Just 0
decode (App _ (Idx 1) (Idx 0))  = Just 1
decode (Abs                λ)   = decode λ
decode (App _ (Idx 1)      r)   = fmap (+1) (decode r)
decode                     _    = Nothing
