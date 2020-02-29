{-# LANGUAGE UnicodeSyntax #-}

module Unlambda.AST 
        ( Aλ (A, E)
        , Eλ (K, Kf, S, Sf, Sff, I, D, R, V)
        ) where

-----------------------------------------------------------------------
----------------------- Datatypes -------------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            λ is beautiful
            Shut up
        ---*-_-*-_-*-_-*---}

-- Application datatype for representing programs as an AST
data Aλ = A Aλ Aλ | E Eλ

-- Datatype for representing the individual combinators
-- This is what is collapsed in the interpreter logic
data Eλ = K
        | Kf Eλ
        | S
        | Sf Eλ
        | Sff Eλ Eλ
        | I
        | D String
        | R
        | V

-----------------------------------------------------------------------
----------------------- Instances -------------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            Show me love
            And pretty strings
        ---*-_-*-_-*-_-*---}

instance Show Aλ where
        show (A a b)   = "`(" ++ show a ++ ")" ++ show b
        show (E e)     = case e of
                           K         -> "k"
                           (Kf a)    -> "kf" ++ show (E a)
                           S         -> "s"
                           (Sf a)    -> "sf" ++ show (E a)
                           (Sff a b) -> "sff" ++ show (E a) ++ show (E b)
                           I         -> "i"
                           (D a)     -> "." ++ a
                           R         -> "r"
                           V         -> "v"
                        -- _         -> "NOT IMPLEMENTED"

instance Show Eλ where
        show K         = "<k>"
        show (Kf a)    = "<k>" ++ show a
        show S         = "<s>"
        show (Sf a)    = show (Sff a I)
        show (Sff a b) = "<s>" ++ show a ++ show b
        show I         = ""
        show (D a)     = "." ++ a
        show R         = "\n"
        show V         = "<v>"
     -- show _         = "NOT IMPLEMENTED"

