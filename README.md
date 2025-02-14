# auto-split 🤸

A GHC plugin that performs automatic case splitting.

### Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.
Here is an example command for starting a REPL for a stack project with the
`auto-split` plugin enabled:

```
stack repl my-project --package auto-split --ghci-options='-fplugin AutoSplit'
```

likewise for a cabal project:

```
cabal repl my-project --build-depends auto-split --repl-options='-fplugin AutoSplit'
```

With the plugin enabled, use `SPLIT` in a pattern match for a case statement or
function declaration. Compiling the module will then result in that pattern
match being expanded to cover all missing cases.

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
been updated. This is done by having GHC emit a custom error. This error does
not indicate that something went wrong.

### Known limitations

- A module must pass the type checker for splitting to occur, unless the
  `-fdefer-type-errors` GHC flag is used.
- Using `SPLIT` in a pattern match will insert patterns for _all_ missing cases
  in the group. It doesn't restrict to the position where `SPLIT` is used.
- If the pattern match where `SPLIT` is being used contains a catch-all
  wildcard case then the plugin will have no effect because there are no
  missing patterns.
- Doesn't work well with the view patterns syntax extension
- Doesn't apply to code inside CPP conditional blocks
- The plugin only supports certain GHC versions with the intent of supporting
  the four latest release series, i.e. `9.6.*` thru `9.12.*`. The cabal file
  indicates the currently supported versions.
