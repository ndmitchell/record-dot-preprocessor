---
author: Neil Mitchell and Shayne Fletcher
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Record Dot Syntax

The "dot operator" for record field selection is absolutely ubiquitous in modern programming languages. Here, we propose a language extension `RecordDotSyntax` that extends Haskell to support record field access via '.' syntax.

## Motivation

Languages that enable projection of a record component via '.' include Fortran, Ada, Pascal, C, C++, Java, OCaml, Erlang, Python and many, many more. The sheer prevalence of this notation is an overwhelming indicator that it suits the semantic of accessing an aggregate remarkably well. The absence of this syntax in Haskell, though non-critical and well meaning gives rise to a "principle of least surprise" violation for newcomers.

This proposal enables record field access via '.' in Haskell. The essence of this proposal is that `a.b` will get the `b` field from the `a` record and many different datatypes can have a `b` field.

Here's a basic example of what is on offer:

```haskell
{-# LANGUAGE RecordDotSyntax #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

In this example, two records are declared both having `name` as a field label. The user may then write `c.name` and `c.owner.name` to access those fields. We can also write `c{name = x}` as a record update, which still works even though `name` is no longer unique.

An implementation of this proposal has been battle tested and hardened over 18 months in enterprise settings in [Digital Asset](https://digitalasset.com/)'s [DAML](https://daml.com/) smart contract language (a Haskell derivative utilizing GHC in its implementation). The feature enjoys universal popularity with users and currently, no adverse interactions with other Haskell language features are known.

## Proposed Change Specification

This change adds a new language extension (enabled at source via `{-# LANGUAGE RecordDotSyntax #-}` or on the command line via the flag `-XRecordDotSyntax`).

The use of '.' to denote function composition is disambiguated from the use of '.' to denote record field access by the presence (, respectively absence,) of whitespace surrounding the '.'.

In the event the language extension is enabled:

* `expr.lbl` is equivalent to `getField @"lbl" expr` (the `.` cannot have whitespace on either side).
* `expr{lbl = val}` is equivalent to `setField @"lbl" expr val`.
* `(.lbl)` is equivalent to `(\x -> x.lbl)` (the `.` cannot have whitespace after).
* `expr{lbl1.lbl2 = val}` is equivalent to `expr{lbl1 = (expr.lbl1){lbl2 = val}}`, performing a nested update.
* `expr{lbl * val}` is equivalent to `expr{lbl = expr.lbl * val}`, where `*` can be any operator.
* `expr{lbl1.lbl2}` is equivalent to `expr{lbl1.lbl2 = lbl2}`.

The above forms combine to provide these identies:

* `expr.lbl1.lbl2` is equivalent to `(expr.lbl1).lbl2`.
* `(.lbl1.lbl2)` is equivalent to `(\x -> x.lbl1.lbl2)`.
* `expr.lbl1{lbl2 = val}` is equivalent to `(expr.lbl1){lbl2 = val}`.
* `expr{lbl1 = val}.lbl2` is equivalent to `(expr{lbl1 = val}).lbl2`.
* `expr{lbl1.lbl2 * val}` is equivalent to `expr{lbl1.lbl2 = expr.lbl1.lbl2 * val}`.
* `expr{lbl1 = val1, lbl2 = val2}` is equivalent to `(expr{lbl1 = val1}){lbl2 = val2}`.

## Examples

Basic examples:

```haskell
data A = A {x :: Int}
data B = B {y :: A, z :: A}
data C = C {a :: Int, b :: Int}

-- Get
f :: A -> Int
f s = s.x

-- Get/set
g :: A -> A
g s = s {x = s.x + 1}

-- Nesting gets and sets
h :: B -> B
h s = s{y = s.y{x = s.y.x + 1}, z = s.z{x = (\ x -> x * x) s.z{xx = s.z.x}.x}}

-- Sections
i :: [A] -> [Int]
i l = map (.x) l

-- Sections with nesting
j :: [B] -> [Int]
j l = map (.y.x) l
```

A fuller, more rigourous set of tests are available in the examples directory of [this repository](https://github.com/ndmitchell/record-dot-preprocessor). Those tests take the following considerations into account:

* Basic operations;
* Parametrically polymorphic datatypes;
* Complex record updates including nested updates;
* Multiple occurences of a field label in a single datatype;
* Infix applications and associativity;
* Interoperability with existing extensions;
* Interoperability with GADTs and existentials.

## Effect and Interactions

This proposal advocates a language extension `RecordDotSyntax` that enables the "record dot" notation traditional in most modern programming languages. Accordingly, the feature is "opt-in". Without regard for whether the extension is enabled or not, we do not anticipate adverse interactions with existing language or compiler features.

## Costs and Drawbacks

Implementation of this proposal is a "light touch". Accomodating the language extension is achievable within a single pass over the parse tree. There are no modifications required with respect to the desguaring, typechecking or code generation phases of the compilation pipeline. The program implementing the extension is short and readable. The effect lowers the barrier to entry of record manipulation for novice users in a meaningful way. There are no known drawbacks.

## Alternatives

The most realistic alternative to this proposed change is to retain the "status quo". That would be a shame since Haskell records are known to be the subject of criticism in some quarters and it's easy to improve upon with respect to this specific issue.

## Unresolved Questions

There are no unresolved questions at this time.

## Implementation Plan

If accepted, the proposal authors would be delighted to provide an implementation. There are no outstanding prerequisities that would need to first be satisfied.
