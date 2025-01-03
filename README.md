# CaseX

A GHC plugin that does automatic case splitting.

**Under developement**

To get started, simply enable the plugin and use `SPLIT` in a pattern match for
a case statement or function declaration. Running GHC will then result in that
pattern match being expanded to cover all missing cases.

For example, compiling this module

```haskell
module Foo where

foo :: Maybe Bool -> String
foo x = case x of
  SPLIT -> undefined
```

results in the module file being updated to

```haskell
module Foo where

foo :: Maybe Bool -> String
foo x = case x of
  Just _ -> undefined
  Nothing -> undefined
```

if the module is then changed to

```haskell
module Foo where

foo :: Maybe Bool -> String
foo x = case x of
  Just SPLIT -> "foo"
  Nothing -> "bar"
```

compiling will result in

```haskell
module Foo where

foo :: Maybe Bool -> String
foo x = case x of
  Just True -> "foo"
  Just False -> "foo"
  Nothing -> "bar"
```

The `SPLIT` pattern can be used in this way anywhere a pattern match group
occurs: case statements, lamba case, or function declarations.

Known caveats:
- Doesn't work well with view patterns
- Using SPLIT in pattern match will insert patterns for _all_ missing
  cases. It doesn't restrict to the position where SPLIT is used, as some users
  might expect.
