# BruSKI

![banner](./BruSKI-banner.jpg?raw=true "Title")

### "DeBruijn to SKI" Untyped language that compiles to Unlambda.
Haskell project that aims to show that small abstractions to lambda calculus yield a surprisingly useful language.
And in some part for me to investigate my interest in the interface between lambda calculus and combinator calculus.

## Installation
Note that this is in alpha, so expect some bugs and missing features. Feel free to open an issue if you find a bug that is not listed!

First, make sure you have stack installed. You can find it here:

https://docs.haskellstack.org/en/stable/README/#how-to-install

Then install with stack. Note that you have to add `~/.local/bin/` to your path.

```
$ stack install
```

## bruc
Bruc (like the name Bruce) is the BruSKI compiler, it is copied to your bin during installation and you should be able to run it in the terminal.

```
$ bruc
      ~  ~          
    ( o )o)         
   ( o )o )o)       
 (o( ~~~~~~~o       
 ( )'~~~~~~~'--.    
 ( )|)       |-- \    BruSKI
   o| /\\    |  \ \   DeBruijn -> SKI
    |  /\\   |   | |  Version 0.4 - June 2020
   o| /  \\/ |  / / 
    |        |-/ /    by Nicklas Botö
    .========.    
```

## Workflow
- [x] [Specification](SPEC.md)
- [x] [Abstract Syntax Tree](src/AST.hs)
- [x] [Lexer](src/Lexer.hs)
- [x] [Parser](src/Parser.hs)
- [x] [Syntactic Sugar / Encodings](src/Encoding.hs)
- [x] [Lambda Translation](src/Generator.hs)
- [x] [Code Generation](src/Generator.hs)
- [x] [Index Escape Delimiter Handling](src/Generator.hs)
- [x] Symbol Table Generation / Management
- [x] Unl Interpreter Integration
- [x] Syntax Highlighting
- [ ] Code Optimization
- [ ] Isolated DSLs
- [ ] Lisp Style Macros

## Specification

BruSKI uses variable abstraction and lambda notation that evaluates to Unlambda.

### Expressions

Expressions are written in de Bruijn indexed lambda calculus. A variable is represented as the number of binders that are in scope of its binder (staring from 0, which is non-standard).

| Standard           | De Bruijn     |
|--------------------|---------------|
| λf.λg.λx.f x (g x) | λλλ 2 0 (1 0) |
| λa.λb.a            | λλ 1          |
| λx.x               | λ 0           |

### Assignment

Expressions can be assigned to identifiers using the _:=_ operator. This will evaluate the expression and place it in a symbol table (explained below). 

Specifying the arity of the function, with the _::_ operator, will control the number of entries to the symbol table.
Omitting this will cause the compiler to generate one entry per bound variable (plus one for the symbol itself).
Note that this is really only useful when you want to restrict the user, by throwing an error when a combinator is not found in the symbol table.

```
x := λ0              :: 1
y := λλ0             :: 2
z := λλλ (1 0 (2 0)) :: 3
```

### Application

Functions are called by their name, with their arguments enclosed in curly brackets.

```
FUNC_NAME{VAR1, VAR2, ...}
```

Note that the number of variables are free to vary, as long as it is less than or equal to the arity of the function.
Not applying the full amount of arguments will result in partial application, and another function is returned.

### The (::) operator
As you might have seen earlier in the file, (::) follow every assignment expresssion. It will assign a arity to the written expression. This is not necessary however, since the compiler will assign the correct arity to the function if (::) is left out. This can be used to restrict the use of encoded expressions by assigning a arity lower that the actual one (assigning one higher is not possible).

Take the successor function in the church encoding. The expression has three binders (where the two latter ones are used for encoding) but we never want the function to accept more than one argument, the number to increment. Therefore we assign it the arity one, with the (::) operator.

```
succ := λλλ (1 (2 1 0)) :: 1
```

### The (!!) operator

Expressions written after the (!!) operator will be evaluated and compiled. Expressions written like this on multiple rows will be concatenated and evaluated as one.

```
true  := λλ1 :: 0
false := λλ0 :: 0
or    := λλ (1 true 0) 

!! or{true, false}

{-
This will evaluate to <k>
equivalent to the 'true' function
-}
```

### Built-ins

Some built-in "syntactic sugar" functions are included, for ease of use. These include functions for encoding types in lambda expressions and language conversions to Unlambda.

#### INT

_INT_ converts natural numbers to their church encoding. Note that concatenations of different encodings will defer to the highest ranked encoder (i.e the outermost encoding).

```
INT{2} => λ ζ{ζ{0}}
```

#### CHR

_CHR_ encodes a character like the Church encoding with its ASCII value being encoded. Note the use of a different successor function ξ, as this encoding is treated differently by the compiler.

```
CHR{d} => λ ξ{ξ{ξ{and so on...}}}
```

#### UNL

Alternatively, expressions can be written in Unlambda using the _UNL_ function.

```
UNL{```.H.i.!i}
```

You can use _%_ to use DeBruijn indeces inside the UNL function.

```
λ UNL{`.%0}
```

#### PRT

Instead of writing the _.x_ unlambda operator in the _UNL_ function, you can use _PRT_ with a string. This string will be converted to an Unlambda function printing it.

```
PRT{BruSKI} => UNL{``````.B.r.u.S.K.Ii}
```

### Comments

The characters _--_ (double dash) denote comments. Multiline comments use the symbol  _{- / -}_.

```
-- Ooga booga
kComb {- the K-combinator -} := λλ 1 :: 2
```

### Example

```
{! import bools !}

--!-- adder example

cin     := F
halfAdd := λλ (xor 0 1)
add     := λλ (xor cin halfAdd{0, 1}) 
cout    := λλ or { (and 0 1)
                 , (and cin halfAdd{0, 1})
                 }

chs := λ (0 UNL{.T} UNL{.F})
out := λ (chs{0} (λ0)) :: 1

!! out {
		add{ F , T } --> T, that is 1
   } 
```

Another, with syntax highlighting!


![example](./example.png?raw=true "Example")
