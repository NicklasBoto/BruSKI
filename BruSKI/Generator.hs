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

-- Stores intermediate expressions
data Iλ = Idx1 Int
        | Abs1 Iλ
        | App1 Iλ Iλ
        | S1
        | K1
        | I1
        deriving (Show, Eq)

-- Stores whether an index is free, bound, or at head position.
-- Note that "head" in this context is like the haskell head,
-- not the head redex in β-reduction.
-- Free here means that an index counts to a binder outside of its scope,
-- and bound means it's inside.
data VarType = Free | Bound | Head deriving (Show, Eq)

-- Converts from the parsed DeBruijn-expressions to their intermediate forms.
toIλ :: Bλ -> Iλ
toIλ (Idx   n) = Idx1 (fromInteger n)
toIλ (Abs   λ) = Abs1 (toIλ λ)
toIλ (App l r) = App1 (toIλ l) (toIλ r)
toIλ        _  = Idx1 0 -- need to handle compiler specific terms

-- Converts intermediate combinators to their SKI forms.
fromIλ :: Iλ -> Aλ
fromIλ S1 = E S
fromIλ K1 = E K
fromIλ I1 = E I
fromIλ (App1 l r) = A (fromIλ l) (fromIλ r)

-- Lists the indeces with their "bindedness".
-- More on this at the VarType definition.
inScope :: Iλ -> [VarType]
inScope x = iS 0 x where
    iS v (Idx1 n)   = [if n <= v-1 then if v-n-1 == 0 then Head else Bound else Free]
    iS v (App1 l r) = iS v l ++ iS v r
    iS v (Abs1 λ)   = iS (v+1) λ
    iS v _          = []

-- Checks if the binder at head position has any indeces bound to it, or not.
headFree, headBound :: Iλ -> Bool
headFree  = none (==Head) . inScope
headBound = not . headFree

-- Decrements all the free indeces in DeBruijn-expressions.
-- This function is really only necessary when translating from DeBruijn.
-- For example, the elimination:
-- T[λx.(S I T[λy.x])] => T[λx.(S I (K T[x]))] 
-- isn't a problem when naming variables, because they don't account for scope.
-- But using DeBruijn indeces:
-- T[λ(S I T[λ1])] =/> T[λ(S I 1)]
-- The index 1 has lost one binder in its scope, so it needs to be decremented.
-- T[λ(S I T[λ1])] => T[λ(S I 0)]
decIndex :: Iλ -> Iλ
decIndex x = dI 0 x where
    dI v (Idx1 n)   = Idx1 $ if n <= v-1 then n else n-1
    dI v (App1 l r) = App1 (dI v l) (dI v r)
    dI v (Abs1 λ)   = Abs1 (dI (v+1) λ)
    dI v ski        = ski

-- Translates DeBruijn λ-terms into SKI-terms by abstraction elimination.
-- The patterns and guard patterns are commented with the respective rules found at
-- https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis
-- Also note that an η-reduction rule is added to shorten the SKI expressions.
-- Some inspiration at https://blog.ngzhian.com/ski2.html, thank you Zhi An Ng.
-- And thanks to @VictorElHajj for some wisdom!
translate :: Iλ -> Iλ
translate (Idx1 x)     = Idx1 x                             -- Rule 1
translate (App1 e1 e2) = App1 (translate e1) (translate e2) -- Rule 2
translate l@(Abs1 λ)
    -- η-reduction
    -- This is essentially checking if a λ-expression can be
    -- written "point free", in haskell terms.
    -- Note that, by using DeBruijn indices,
    -- we have to decrement the free indicies in λ. 
    -- from https://en.wikipedia.org/wiki/Combinatory_logic#Simplifications_of_the_transformation
    | App1 e1 (Idx1 _) <- λ
    , last (inScope l) == Head && headFree (Abs1 e1)
    = translate (decIndex e1)
    -- Rule 3
    -- Here too, we need to decrement.
    -- Converts to the K combinator.
    | headFree l 
    = App1 K1 (translate (decIndex λ))
    -- Rule 4
    -- Converts to the I combinator.
    | Idx1 0 <- λ 
    = I1
    -- Rule 5
    -- Translates the body of an abstraction.
    | Abs1 _ <- λ
    , headBound l
    = translate (Abs1 (translate λ))
    -- Rule 6
    -- Converts to the S combinator
    | App1 e1 e2 <- λ
    , headBound (Abs1 e1) || headBound (Abs1 e2)
    = App1 (App1 S1 (translate (Abs1 e1))) (translate (Abs1 e2))
-- Trivial cases
translate S1 = S1
translate K1 = K1
translate I1 = I1
translate _  = error "Not implemented!"

-- Turns translated expressions to Unlambda code.
generate :: Iλ -> String
generate λ = g (fromIλ λ) where
    g (E S) = "s"
    g (E K) = "k"
    g (E I) = "i"
    g (A l r) = '`' : g l ++ g r