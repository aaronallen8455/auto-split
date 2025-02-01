# auto-split

A GHC plugin that does automatic case splitting.

Simply enable the plugin and use `SPLIT` in a pattern match for a case
statement or function declaration. Compiling the module will then result in
that pattern match being expanded to cover all missing cases.

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

If case splitting results in constructors or patterns that are not in scope,
they will be qualified with `NOT_IN_SCOPE`.

Compilation will abort when case splitting occurs since the module file has
been updated. This is done by having GHC emit a custom error.

### Usage

This plugin is intended to be used with GHCi or related utilities such as
`ghcid` and `ghciwatch` rather than as a package dependency. Here is an example
of how to use it in the REPL for a stack project:

```
stack repl my-project --package auto-split --ghci-options='-fplugin AutoSplit'
```

or a cabal project:

```
cabal repl my-project --build-depends auto-split --repl-options='-fplugin AutoSplit'
```

### Known limitations

- A module must pass the type checker for splitting to occur, unless the
  `-fdefer-type-errors` GHC flag is used.
- Using SPLIT in pattern match will insert patterns for _all_ missing cases in
  the group. It doesn't restrict to the position where SPLIT is used.
- Doesn't work well with view patterns
- Doesn't apply to code inside CPP conditional blocks
- The plugin only supports certain GHC versions with the intention of supporting
  the four latest major releases. Check the cabal file for specifics.
