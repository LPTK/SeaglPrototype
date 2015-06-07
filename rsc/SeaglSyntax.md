# Basic Syntax – Incremental Definition

The idea is to quickly have a simple syntax that allows us to write nice things. We'll see later for syntax sugar and advanced features.

## Tentative State

### Iteration 1

Be able to "let"-bind both value and type names to their definitions, and stream them. Type lets should be recognized by the name that starts with a capital letter.

```
foo = bar x y; T = List Int; print 42; ()
```

*AST question*: Blocks? They may make it easier to represent this mix of type and value declarations...

