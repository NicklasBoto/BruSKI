# TODO (But not really)
Ok I know, this is not really a TODO, this is mostly filled with ideas and such. But this filename got a really nice icon in vs code.

## Macros, dude
And maybe some other {C / C} style for macro implementation and such. Where C is some character, like '-' for comments and '*' in the example above.

```
{+ macro_name
macro_body
+}
```

maybe

```
{+ $a + $b
add{a,b}
+}

{+ [$a, $b]
cons{$a,cons{$b, nil}}
+}

{+ #$a
INT{$a}
+}

{+ @$a
CHR{$a}
+}

{+ $a+$b
{! import church.add !}
!! add{INT{$a}, INT{$b}}
+}

-- with haskell
{+
l <- natural
char '+'
r <- natural
return $ Fun "add" (map (encode toInt) [l, r])
+}

```

I'm not sure whether the macro body should be defined in Haskell (with code injection), or BruSKI.

Also...
You should be able to choose how to delimiterize your macro arguments

```
{+ #include "$f" ; {! import $f !} +}
-- which would generate the macro for
#include "church"
```

## Alpha naming abstractions
Hey, I like DeBruijn as much as the next guy, but... naming variables __can__ be a good idea.
At least as comments after the lambdas. Kinda like how you can comment:

commented := λ {-x-} 0

But with some syntacic sugaring.

Ideas:

```
λ'x
λx!
λ:x
λ@x
λx. -- This, obviously

commented := λx.0
```

## Pattern Matching
Kinda Haskell-like pattern matching.
I might have to implement some Eq function in the compiler though...

```
div       :: 2
div{_,n0} := UNL{eZ}
div{_,n1} := λλ1
div{n1,_} := n0
div{n0,_} := n0
div       := λλ add{ n1,
                div{ sub{1, 0}, 0}} -- and recursion
```

Which would be the macro:

```
foo     :: 1
foo{n0} := n0
foo     := n1

-- expands to

foo := λ (
    {-if  -} EQ{0, n0}
    {-then-} n0
    {-else-} n1
    )
```
