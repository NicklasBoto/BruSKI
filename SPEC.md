## BruSKI, DeBruijn to SKI

BruSKI (p. brew-ski) uses variable abstraction and lambda notation that evaluates to Unlambda.

### Expressions

Expressions are written in de Bruijn indexed lambda calculus. A variable is represented as the number of binders that are in scope of its binder (staring from 0, which is non-standard).

| Standard           | De Bruijn     |
|--------------------|---------------|
| λf.λg.λx.f x (g x) | λλλ 2 0 (1 0) |
| λa.λb.a            | λλ 1          |
| λx.x               | λ 0           |

### Variables

Expressions can be bound to variables using the _:=_ operator. This will evaluate the expression and place it in a symbol table (explained below). 

Specifying the arity of the function, with the _::_ operator, will control the number of entries to the symbol table.
Omitting this will cause the compiler to generate one entry per bound variable (plus one for the symbol itself).
Note that this is really only useful when you want to restrict the user, by throwing an error when a combinator is not found in the symbol table.

```
x := λ 0         :: 1
y := λλ 0        :: 2
z := λλλ 0 (2 3) :: 3
```

### Evaluation

The compiler creates a symbol table of variable names and their associated Unlambda expressions, as well as the partially applied combinators.

```
kComb := λλ 1
```

This would be evaluated to the symbol table:

| Variable Name | Unλ expression |
|---------------|----------------|
| \_\_kComb     | k              |
| f\_\_kComb    | \`k            |
| ff\_\_kComb   | \`\`k          |

In this expression, _λλ1_ is bound to the name kComb and is given two applications kCombf and kCombff (due to the 2 assignment)

### Application

Functions are called by their name, with their arguments enclosed in curly brackets.

```
FUNC_NAME{VAR1, VAR2, ...}
```

Note that the number of variables are free to vary, as long as it is less than or equal to the arity of the function.
Not applying the full amount of arguments will result in partial application, and another function is returned.

Using indices as arguments is done by denoting them with the symbol _%_, called a SKID-mark (which of course means SKI delimiter). 

```
λλ FUNC_NAME{%0, %1, ...}
```

### Built-ins

Some built-in "syntactic sugar" functions are included, for ease of use. These include functions for encoding types in lambda expressions and language conversions to Unlambda.

#### INT

_INT_ converts natural numbers to and from their church encoding.

```
INT{2} => λ ζ{ζ{%0}} => λ ζ (ζ 0)
```

#### CHR

_CHR_ encodes a character similar to a church encoding. With 'a' being 0, 'b' being '1', and so on. Capital letters are 29 and onward.

```
CHR{d} => λ ξ{ξ{ξ{%0}}} => λ ξ (ξ (ξ 0))
```

#### UNL

Alternatively, expressions can be written in Unlambda using the _UNL_ function.

```
UNL{```.H.i.!i}
```

### Comments

The character _;_ (semicolon) denotes comments. Inline comments use the symbol  _;* / *;_.

```
; Ooga booga
kComb ;* the K-combinator *; := λλ 1 :: 2
```

### Example

```
; Stupid complicated way of printing

kComb := λλ 1                  :: 2
print := λ UNL{`.%0i}          :: 1
foo   := kComb{CHR{y}, CHR{n}} :: 0 

print{foo}

;*
This evaluates to "`.yi", in Unlambda.
Which prints "y".
*;
```

