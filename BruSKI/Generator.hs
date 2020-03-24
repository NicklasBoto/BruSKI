module Generator
        (
        ) where

---- Language Import
import AST
import Unlambda.AST

---- Parser Import
import Parser
import Unlambda.Parser

---- Debug Import
import Unlambda.Run

---- Error Import
import Data.Maybe
import Control.Lens

---- Intermediate datatype for storing peri-compile expressions
data Inter = BRU Bλ 
           | To  Inter Inter
           | SKI Aλ

instance Show Inter where
        show (BRU   b) = show b
        show (SKI   s) = show s
        show (To  l r) = "T[" ++ show l ++ ", " ++ show r ++ "]"

-- Basically a True/False for variables being bound, but might need a third Coded value.
data VarType = Free | Bound | Coded deriving (Show, Eq)

-- Checks if the x:th index in a Bλ-expression is bound to any λ
isFree :: Integer -> Bλ -> Bool
isFree x λ = case inScope λ ^? element (fromInteger x) of
    Just  j -> j == Free
    Nothing -> error $ show x ++ " is not an index in expression: " ++ show λ

-- Does the same thing as isFree but with named variables
-- Might not be used
isFree1 :: Char -> Bλ -> Bool
isFree1 c λ = case lookup c (alphaName λ) of
    Just  j -> j == Free
    Nothing -> error $ c : " is not a variable in expression: " ++ show λ

-- Abstraction levels of DeBruijn indeces
-- Might not be used
abstractionLevel :: Bλ -> [Integer]
abstractionLevel x = aL 0 x where
    aL v (Idx _)               = [v]
    aL v (App (Abs l) (Abs r)) = aL (v+1) l ++ aL (v+1) r
    aL v (App (Abs λ) r)       = aL (v+1) λ ++ aL v r
    aL v (App l (Abs λ))       = aL v l ++ aL (v+1) λ
    aL v (App l r)             = aL v l ++ aL v r
    aL v (Abs λ)               = aL v λ
    aL v _                     = []

-- Similar to abstractionLevel but checks if a index N has a binder N steps away in its scope 
inScope :: Bλ -> [VarType]
inScope x = iS 0 x where
    iS v (Idx n)               = [if n <= v-1 then Bound else Free]
    iS v (Prc n)               = [if n <= v-1 then Bound else Free]
    iS v (App (Abs l) (Abs r)) = iS (v+1) l ++ iS (v+1) r
    iS v (App (Abs λ) r)       = iS (v+1) λ ++ iS v r
    iS v (App l (Abs λ))       = iS v l ++ iS (v+1) λ
    iS v (App l r)             = iS v l ++ iS v r
    iS v (Abs λ)               = iS (v+1) λ
    iS v _                     = [] 

-- Names the indeces in a DeBruijn-expression so that becomes equivalent to a λ-expression
-- Might not be used
alphaName :: Bλ -> [(Char, VarType)]
alphaName = zipWith (,) ['a'..] . inScope
