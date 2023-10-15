# record-dot-preprocessor [![Hackage version](https://img.shields.io/hackage/v/record-dot-preprocessor.svg?label=Hackage)](https://hackage.haskell.org/package/record-dot-preprocessor) [![Stackage version](https://www.stackage.org/package/record-dot-preprocessor/badge/nightly?label=Stackage)](https://www.stackage.org/package/record-dot-preprocessor) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/record-dot-preprocessor/ci.yml?branch=master)](https://github.com/ndmitchell/record-dot-preprocessor/actions)

In almost every programming language `a.b` will get the `b` field from the `a` data type, and many different data types can have a `b` field. The reason this feature is ubiquitous is because it's _useful_. The `record-dot-preprocessor` brings this feature to modern GHC versions. This feature has been [proposed for Haskell](https://github.com/ghc-proposals/ghc-proposals/pull/282) as `RecordDotSyntax`. Since GHC 9.2 the [`OverloadedRecordDot`](https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/exts/overloaded_record_dot.html#extension-OverloadedRecordDot) and [`OverloadedRecordUpdate`](https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/exts/overloaded_record_update.html) extensions implement much the same functionality. Some examples:

```haskell
data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

Here we declare two records both with `name` as a field, then write `c.name` and `c.owner.name` to get those fields. We can also write `c{name = x}` as a record update, which still works even though `name` is no longer unique.

## How do I use this magic?

First install `record-dot-preprocessor` with either `stack install record-dot-preprocessor` or `cabal update && cabal install record-dot-preprocessor`. Then at the top of the file add:

* Either: `{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}` for the preprocessor.
* Or: `{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}` and `{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GADTs #-}` for the GHC plugin.

The GHC plugin only runs on GHC 8.6 or higher, [has some issues on Windows](https://gitlab.haskell.org/ghc/ghc/issues/16405) and has much better error messages. In contrast, the preprocessor runs everywhere and has more features.

You must make sure that the `OPTIONS_GHC` is applied both to the file _where your records are defined_, and _where the record syntax is used_. The resulting program will require the [`record-hasfield` library](https://hackage.haskell.org/package/record-hasfield).

## What magic is available, precisely?

Using the preprocessor or the GHC plugin you can write:

* `expr.lbl` is equivalent to `getField @"lbl" expr` (the `.` cannot have whitespace on either side).
* `expr{lbl = val}` is equivalent to `setField @"lbl" expr val` (the `{` cannot have whitespace before it).
* `(.lbl)` is equivalent to `(\x -> x.lbl)` (the `.` cannot have whitespace after).

Using the preprocessor, but _not_ the GHC plugin:

* `expr{lbl1.lbl2 = val}` is equivalent to `expr{lbl1 = (expr.lbl1){lbl2 = val}}`, performing a nested update.
* `expr{lbl * val}` is equivalent to `expr{lbl = expr.lbl * val}`, where `*` can be any operator.
* `expr{lbl1.lbl2}` is equivalent to `expr{lbl1.lbl2 = lbl2}`.

These forms combine to offer the identities:

* `expr.lbl1.lbl2` is equivalent to `(expr.lbl1).lbl2`.
* `(.lbl1.lbl2)` is equivalent to `(\x -> x.lbl1.lbl2)`.
* `expr.lbl1{lbl2 = val}` is equivalent to `(expr.lbl1){lbl2 = val}`.
* `expr{lbl1 = val}.lbl2` is equivalent to `(expr{lbl1 = val}).lbl2`.
* `expr{lbl1.lbl2 * val}` is equivalent to `expr{lbl1.lbl2 = expr.lbl1.lbl2 * val}`.
* `expr{lbl1 = val1, lbl2 = val2}` is equivalent to `(expr{lbl1 = val1}){lbl2 = val2}`.

## How does this magic compare to other magic?

Records in Haskell are well known to be [pretty lousy](https://www.yesodweb.com/blog/2011/09/limitations-of-haskell). There are [many proposals](https://wiki.haskell.org/Extensible_record) that aim to make Haskell records more powerful using dark arts taken from type systems and category theory. This preprocessor aims for simplicity - combining existing elements into a coherent story. The aim is to do no worse than Java, not achieve perfection.

## Any advice for using this magic?

The most important consideration is that all records used by `a.b` or `a{b=c}` syntax _must_ have `HasField` instances, which requires either running the preprocessor/plugin over the module defining them, or writing orphan instances by hand. To use records which don't have such instances use normal selector functions (e.g. `b a`) and insert a space before the `{` (e.g. `a {b=c}`).

## Limitations

* The preprocessor doesn't deal with anti-quoted expressions inside `QuasiQuotes`, e.g. `[D.pgSQL|$ SELECT ${dummy.x} :: text|]`.
