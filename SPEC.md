__SKID, SKI with Delimiters__

SKID uses variable abstraction and de Bruijn indexed lambda notation that evaluates to Unlambda.
The compiler creates a symbol table of variable names and their associated Unlambda expressions, as well as the partially applied combinators.

For example:

```
name := λλ1 :: Binary
```

This would be evaluated to the symbol table:

| Variable Name | Uλ expression |
|---------------|---------------|
| kComb         | k             |
| kCombf        | `k            |
| kCombff       | ``k           |

