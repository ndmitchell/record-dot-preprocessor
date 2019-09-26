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

Records in Haskell are [widely recognised](https://www.yesodweb.com/blog/2011/09/limitations-of-haskell) as being under-powered, with duplicate field names being particularly troublesome. We propose a new language extension `RecordDotSyntax` that provides syntactic sugar to make the features introduced in [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) more accessible, improving the user experience.

## Motivation

In almost every programing language we write `a.b` to mean the `b` field of the `a` record expression. In Haskell that becomes `b a`, and even then, only works if there is only one `b` in scope. Haskell programmers have struggled with this weakness, variously putting each record in a separate module with qualified imports, or prefixing record fields with the type name. We propose bringing `a.b` to Haskell, which works regardless of how many `b` fields are in scope. Here's a simple example of what is on offer:

```haskell
{-# LANGUAGE RecordDotSyntax #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

We declare two records both having `name` as a field label. The user may then write `c.name` and `c.owner.name` to access those fields. We can also write `c{name = x}` as a record update, which works even though `name` is no longer unique.

An implementation of this proposal has been battle tested and hardened over 18 months in the enterprise environment as part of [Digital Asset](https://digitalasset.com/)'s [DAML](https://daml.com/) smart contract language (a Haskell derivative utilizing GHC in its implementation), and also in a [Haskell preprocessor and a GHC plugin](https://github.com/ndmitchell/record-dot-preprocessor/). When initially considering Haskell as a basis for DAML, the inadequacy of records was considered the most severe problem, and without devising the scheme presented here, we wouldn't be using Haskell. The feature enjoys universal popularity with users.

## Proposed Change Specification

FIXME: This change specification needs sections on parsing, desugaring and other bits. It needs to be a lot more technical, ideally as grammar changes to the Haskell report.

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

FIXME: I don't find this compelling. But suggest sorting out the proposed change spec first.

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

FIXME: These examples don't include function updates or nested updates.

A fuller, more rigourous set of tests are available in the examples directory of [this repository](https://github.com/ndmitchell/record-dot-preprocessor). Those tests take the following considerations into account:

* Basic operations;
* Parametrically polymorphic datatypes;
* Complex record updates including nested updates;
* Multiple occurences of a field label in a single datatype;
* Infix applications and associativity;
* Interoperability with existing extensions;
* Interoperability with GADTs and existentials.

## Effect and Interactions

**Polymorphic updates:** When enabled, this extension takes the `a{b=c}` syntax and uses it to mean `setField`. The biggest difference a user is likely to experience is that the resulting type of `a{b=c}` is the same as the type `a` - you _cannot_ change the type of the record by updating its fields. The removal of polymorphism is considered essential to preserve decent type inference.

**Stealing a.b syntax:** The `a.b` syntax is commonly used in conjunction with the `lens` library, e.g. `expr^.field1.field2`. Treating then `a.b` without spaces as a record selection would break such code. The alternatives would be to use a library with a different lens composition operator (e.g. `optics`), or simply not enable this extension when also using lenses. In general those people who are already using lens approaches don't feel the problems of inadequate records as strongly.

## Costs and Drawbacks

This proposal advocates a different style of writing Haskell records, which is distinct from the existing style. As

Implementation of this proposal is a "light touch". Accomodating the language extension is achievable within a single pass over the parse tree. There are no modifications required with respect to the desguaring, typechecking or code generation phases of the compilation pipeline. The program implementing the extension is short and readable. The effect lowers the barrier to entry of record manipulation for novice users in a meaningful way. There are no known drawbacks.

## Alternatives

The primary alternatives to the problem of records are:

* Using the `lens` library. Then concept of lenses is very powerful, but that power can be more complex to use - in many ways lenses let you abstract over record fields.
* Using `DuplicateRecordFields` - although I can find very little usage of this mechanism on Hackage. It seems hard to use in practice.
* Put each record in a separate module - it's done, but it's complex.
* Prefix each field by the name of its type.

All these approaches are currently used, and represent the "status quo", where Haskell records are considered not fit for purpose.

## Unresolved Questions

There are no unresolved questions at this time.

## Implementation Plan

If accepted, the proposal authors would be delighted to provide an implementation. Implementation depends on the implementation of [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) and [`NoFieldSelectors`](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst).
