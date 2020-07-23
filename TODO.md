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

{+ from=[ to=] sep=,
for arg do
    cons{$,
endfor
endwith nil <+> for $ do }
+}

{+ #$a
INT{$a}
+}

{+ @$a
CHR{$a}
+}

{+ (+)

{! import church !}
__0__ := INT{$0}
__1__ := INT{$1}
!! add{ __0__ , __1__ }

+}

-- 2 + 2 -> INT{4}

```

I'm not sure whether the macro body should be defined in Haskell (with code injection), or BruSKI.

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
```

## Pattern Matching
Kinda Haskell-like pattern matching..
I might have to implement some Eq function in the compiler though..

```
div       :: 2
div{_,n0} := UNL{eZ}
div{_,n1} := λλ1
div{n1,_} := n0
div{n0,_} := n0
div       := λλ add{ n1,
                div{ sub{1, 0}, 0}}
```

Which would be the macro:

```
foo :: 1
foo{n0} := n0
foo     := n1

-- expands to

foo := λ (
    {-if  -} EQ{0, n0}
    {-then-} n0
    {-else-} n1
    )
```






