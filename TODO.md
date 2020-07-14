# TODO (But not really)
Ok I know, this is not really a TODO, this is mostly filled with ideas and such. But this filename got a really nice icon in vs code.

## Change indentifier characters?
Consider indentifier names. What should the indentStart and indentLetter (in src/Lexer.hs) be? Names like + and * are cool (maybe haskell style infix operators...), but require parser rework.

## Pre-compile syntax style
Change parser to use more "haskell style" syntax, for example the imports statements now use ` #import <file_name> `, which is ugly but make parsing easier.
I want to minimize the amount of reserved keywords, so maybe a pre-compiler section in the program. This pre-compile stage might be in line with the planned typed dialect syntax.

```
{*

import file1
import file2

*}
```

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

λ'x
λx!
λ:x
λ@x
λx.

## format language definition
Would be nice to have som syntactic sugar for formatting functions, since I always seem to need one...
Instead of defining a function and then appying it add the evaluation, we can define keyword in the 
language definition IDSL, like:

-- Instead of
out := λ (0 UNL{.*} λ0)

!! out{ λλ0 }

-- Something like this
{! format λ (0 UNL{.*} λ0) !}

!! λλ0

That is, a function that is always called first at the evaluator.
