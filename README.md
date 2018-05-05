# record-dot-preprocessor [![Hackage version](https://img.shields.io/hackage/v/record-dot-preprocessor.svg?label=Hackage)](https://hackage.haskell.org/package/record-dot-preprocessor) [![Stackage version](https://www.stackage.org/package/record-dot-preprocessor/badge/lts?label=Stackage)](https://www.stackage.org/package/record-dot-preprocessor) [![Build Status](https://img.shields.io/travis/ndmitchell/record-dot-preprocessor.svg)](https://travis-ci.org/ndmitchell/record-dot-preprocessor)

In almost every programming language `a.b` will get the `b` field from the `a` data type, and many different data types can have a `b` field. The reason this feature is ubiquitous is because it's _useful_. The `record-dot-preprocessor` brings this feature to Haskell. As some examples:

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

First install `record-dot-preprocessor` with either `stack install record-dot-preprocessor` or `cabal update && cabal install record-dot-preprocessor`. Then add `{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}` to the top of the file. Suddenly your records will work. You must make sure that the preprocessor is applied both to the file where your records are defined, and where the record syntax is used.

The resulting program will require the [`lens` library](https://hackage.haskell.org/package/lens).

## What magic is available, precisely?

* `e.b`, where `e` is an expression (not a constructor) and there are no whitespace on either side of the `.`, is translated to a record lookup. If you want to use the standard `.` function composition operator, insert a space. If you want to use a qualfied module name, then `e` will look like a constructor, so it won't clash.
* `e{b = c}` is a record update. Provided the record was defined in a module where `record-dot-preprocessor` was used, the meaning will be equivalent to before. If you want to use a normal unchanged record update, insert a space before the `{`.
* `e{b * c}`, where `*` is an arbitrary operator, is equivalent to `e{b = e.b * c}`. If you want to apply an arbitrary function as `c`, use the `&` operator.
* Record field must all be names, not operators.

## I don't believe in magic, what's the underlying science?

On the way back from [ZuriHac 2017](https://2017.zurihac.info/) [Neil Mitchell](https://ndmitchell.com) and [Mathieu Boespflug](https://www.tweag.io/contact) were discussing lenses and the sad state of records in Haskell. We both agreed that overloaded labels should be defined such that they resolve to lenses. With the right instances, you could define `a ^. #foo` to get the `foo` field from the expression `a`. This preprocessor just turns `a.foo` into `a ^. #foo`, and generates the right instances. If you really want to see the magic under the hood simply run `record-dot-preprocessor yourfile.hs` and it will print out what it generates.
