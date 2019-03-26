# record-dot-preprocessor [![Hackage version](https://img.shields.io/hackage/v/record-dot-preprocessor.svg?label=Hackage)](https://hackage.haskell.org/package/record-dot-preprocessor) [![Stackage version](https://www.stackage.org/package/record-dot-preprocessor/badge/nightly?label=Stackage)](https://www.stackage.org/package/record-dot-preprocessor) [![Build status](https://img.shields.io/travis/ndmitchell/record-dot-preprocessor/master.svg?label=Build)](https://travis-ci.org/ndmitchell/record-dot-preprocessor)

In almost every programming language `a.b` will get the `b` field from the `a` data type, and many different data types can have a `b` field. The reason this feature is ubiquitous is because it's _useful_. The `record-dot-preprocessor` brings this feature to Haskell. Some examples:

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
* Or: `{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}` and `{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds #-}` for the GHC plugin.

The GHC plugin only runs on GHC 8.6 or higher, doesn't work on Windows, has better error messages and is less likely to break your source file. In contrast, the preprocessor runs everywhere and has more features.

You must make sure that the `OPTIONS_GHC` is applied both to the file where your records are defined, and where the record syntax is used. The resulting program will require the [`record-hasfield` library](https://hackage.haskell.org/package/record-hasfield).

## What magic is available, precisely?

Using the preprocessor or the GHC plugin you can write:

* `expr.lbl` is equivalent to `getField @"lbl" expr` (the `.` cannot have whitespace on either side).
* `expr{lbl = val}` is equivalent to `setField @"lbl" expr val`.
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
* `expr{lbl1.lbl2 * val}` is equivalent to `expr{lbl1.lbl2 = expr.lbl1.lbl2 * val}`

## How does this magic compare to other magic?

Records in Haskell are well known to be [pretty lousy](https://www.yesodweb.com/blog/2011/09/limitations-of-haskell). There are [many proposals](https://wiki.haskell.org/Extensible_record) that aim to make Haskell records more powerful using dark arts taken from type systems and category theory. This preprocessor aims for simplicity - combining existing elements into a coherent story. The aim is to do no worse than Java, not achieve perfection.
