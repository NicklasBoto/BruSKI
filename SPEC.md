## SKID, SKI with Delimiters

SKID uses variable abstraction and lambda notation that evaluates to Unlambda.

### Expressions

Expressions are written in de Bruijn indexed lambda calculus. A variable is represented as the number of binders that are in scope of its binder (staring from 0, which is non-standard).

| Standard           | De Bruijn     |
|--------------------|---------------|
| λf.λg.λx.f x (g x) | λλλ 2 0 (1 0) |
| λa.λb.a            | λλ 1          |
| λx.x               | λ 0           |

Alternatively, expressions can be written in unlambda using the _UNL_ function.

```
UNL{```.H.i.!i}
```

### Variables

Expressions can be bound to variables using the _:=_ operator. This will evaluate the expression and place it in a symbol table (explained below). 

Specifying the arity of the function will control the number of entries to the symbol table.
Omitting this will cause the compiler to generate one entry per bound variable (plus one for the symbol itself).

```
x := λ 0         :: Unary
y := λλ 0        :: Binary
z := λλλ 0 (2 3) :: Tertiary
```

### Evaluation

The compiler creates a symbol table of variable names and their associated Unlambda expressions, as well as the partially applied combinators.

```
kComb := λλ 1
```

This would be evaluated to the symbol table:

| Variable Name | Unλ expression |
|---------------|----------------|
| kComb         | k              |
| kCombf        | \`k            |
| kCombff       | \`\`k          |

In the expression _λλ1_ is bound to the name kComb and is given two applications kCombf and kCombff (due to the Binary assignment)

### Application

Functions are applied using the following notation:

```
FUNC_NAME{VAR1, VAR2, ...}
```

Note that the number of variables are free to vary, as long as it is less than or equal to the arity of the function.
Not applying the full amount of arguments will result in partial application, and another function is returned.

