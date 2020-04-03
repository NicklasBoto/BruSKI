{-# LANGUAGE UnicodeSyntax #-}

module Generator
        (
        ) where

{-
I don't drink alcohol,
I drink beer.
    - Alaska State Troopers S4 E8
-}

---- Language Import
import AST
import Unlambda.AST

---- Parser Import
import Parser
import Unlambda.Parser

---- Debug Import
import Unlambda.Run
import Debug.Trace

---- Error Import
import Data.Maybe
import Control.Lens
import Control.Applicative ((<|>))

data Iλ = Idx1 Int
        | Abs1 Iλ
        | App1 Iλ Iλ
        | S1
        | K1
        | I1
        | T Iλ Iλ
        deriving (Show, Eq)

-- Stores whether an index is free, bound, or at head redex
data VarType = Free | Bound | Head deriving (Show, Eq)

mediate :: Bλ -> Iλ
mediate (Idx   n) = Idx1 (fromInteger n)
mediate (Abs   λ) = Abs1 (mediate λ)
mediate (App l r) = App1 (mediate l) (mediate r)
mediate        _  = Idx1 0

headFree, headBound :: Iλ -> Bool
headFree  = none (==Head) . inScope
headBound = not . headFree

{-
-- Checks if the x:th index in a Bλ-expression is bound to any λ
isFree :: Int -> Iλ -> Maybe Bool
isFree x λ = case inScope λ ^? element x of
    Just y -> y == Just Head || y == Just Free
    Nothing -> Nothing

isFree' :: Int -> Iλ -> Maybe Bool
isFree' x λ = case drop x (inScope λ) of
    y:_ -> y == Just Head || y == Just Free
    []  -> Nothing
-}

-- Similar to abstractionLevel but checks if a index N has a binder N steps away in its scope 
inScope :: Iλ -> [VarType]
inScope x = iS 0 x where
    iS v (Idx1 n)                 = [if n <= v-1 then if v-n-1 == 0 then Head else Bound else Free]
    iS v (App1 (Abs1 l) (Abs1 r)) = iS (v+1) l ++ iS (v+1) r
    iS v (App1 (Abs1 λ) r)        = iS (v+1) λ ++ iS v r
    iS v (App1 l (Abs1 λ))        = iS v l ++ iS (v+1) λ
    iS v (App1 l r)               = iS v l ++ iS v r
    iS v (Abs1 λ)                 = iS (v+1) λ
    iS v _                        = [] 

-- Names the indeces in a DeBruijn-expression so that becomes equivalent to a λ-expression
-- Might not be used
alphaName :: Iλ -> [(Char, VarType)]
alphaName = zip ['a'..] . inScope

decIndex :: Iλ -> Iλ
decIndex (Idx1 0) = error "Index at zero"
decIndex (Idx1 n) = Idx1 (n-1)
decIndex       λ  = λ

tx1 :: Iλ -> Iλ
tx1 (Idx1 x)     = trace ("1: " ++ show (Idx1 x)) $ Idx1 x
tx1 (App1 e1 e2) = trace ("2: " ++ show (App1 e1 e2)) $ App1 (tx1 e1) (tx1 e2)
tx1 l@(Abs1 λ)
    | headFree l                     = trace ("3: " ++ show l) $ App1 K1 (tx1 λ)
    | λ == Idx1 0                    = trace ("4: " ++ show l) $ I1
    | (Abs1 e)     <- λ, headBound l = trace ("5: " ++ show l) $ tx1 (Abs1 (tx1 λ))
    | (App1 e1 e2) <- λ, headBound (Abs1 e1) || headBound (Abs1 e2)
    = trace ("6: " ++ show l) $ App1 (App1 S1 (tx1 (Abs1 e1))) (tx1 (Abs1 e2))
tx1 S1 = S1
tx1 K1 = K1
tx1 I1 = I1
--tx1 a  = error $ show a

-- Fix this false poop
-- run tx1 test

test3 = Abs1 (Idx1 1)
test5 = undefined
test = Abs1 . Abs1 $ App1 (Idx1 0) (Idx1 1)
test' = Abs1 (App1 (Abs1 $ Idx1 0) (Idx1 0))


{-
tx :: Inter -> Inter
tx (BRU x) = case x of
    Idx n -> BRU $ Idx n
    App l r -> T (tx $ BRU l) (tx $ BRU r)
    Abs λ -> if not $ headFree λ then T (SKI K) (tx $ BRU λ) else error "NOP"
    Abs (Idx 0) -> SKI I
--  Abs r@(Abs x) -> if headFree r then tx (BRU)
    Abs (App l r) -> if headFree l || headFree r then T (T (SKI S) (tx (BRU $ Abs l))) (tx (BRU $ Abs r)) else error "NOP2"
-}
--tx1 x = 
    