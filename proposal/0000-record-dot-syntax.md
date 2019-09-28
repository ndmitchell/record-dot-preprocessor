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

In almost every programming language we write `a.b` to mean the `b` field of the `a` record expression. In Haskell that becomes `b a`, and even then, only works if there is only one `b` in scope. Haskell programmers have struggled with this weakness, variously putting each record in a separate module and using qualified imports, or prefixing record fields with the type name. We propose bringing `a.b` to Haskell, which works regardless of how many `b` fields are in scope. Here's a simple example of what is on offer:

```haskell
{-# LANGUAGE RecordDotSyntax #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

We declare two records both having `name` as a field label. The user may then write `c.name` and `c.owner.name` to access those fields. We can also write `c{name = x}` as a record update, which works even though `name` is no longer unique. Under the hood, we make use of `getField` and `setField` from [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst).

An implementation of this proposal has been battle tested and hardened over 18 months in the enterprise environment as part of [Digital Asset](https://digitalasset.com/)'s [DAML](https://daml.com/) smart contract language (a Haskell derivative utilizing GHC in its implementation), and also in a [Haskell preprocessor and a GHC plugin](https://github.com/ndmitchell/record-dot-preprocessor/). When initially considering Haskell as a basis for DAML, the inadequacy of records was considered the most severe problem, and without devising the scheme presented here, we wouldn't be using Haskell. The feature enjoys universal popularity with users.

## Proposed Change Specification

FIXME: This change specification needs sections on parsing, desugaring and other bits. It needs to be a lot more technical, ideally as grammar changes to the Haskell report.

This change adds a new language extension (enabled at source via `{-# LANGUAGE RecordDotSyntax #-}` or on the command line via the flag `-XRecordDotSyntax`).

The use of '.' to denote function composition is disambiguated from the use of '.' to denote record field access by the presence (, respectively absence,) of whitespace surrounding the '.'.

In the event the language extension is enabled:

| Expression | Equivalent |
| -- | -- |
| `e.lbl` | `getField @"lbl" e` the `.` cannot have whitespace on either side) |
| `e{lbl = val}` | `setField @"lbl" e val` |
| `(.lbl)` | `(\x -> x.lbl)` the `.` cannot have whitespace after) |
| `e{lbl1.lbl2 = val}` | `e{lbl1 = (e.lbl1){lbl2 = val}}` performing a nested update |
| `e{lbl * val}` | `e{lbl = e.lbl * val}` where `*` can be any operator |
| `e{lbl1.lbl2}` | `e{lbl1.lbl2 = lbl2}` |

The above forms combine to provide these identities:

| Eession | Equivalent
| -- | -- |
| `e.lbl1.lbl2` | `(e.lbl1).lbl2` |
| `(.lbl1.lbl2)` | `(\x -> x.lbl1.lbl2)` |
| `e.lbl1{lbl2 = val}` | `(e.lbl1){lbl2 = val}` |
| `e{lbl1 = val}.lbl2` | `(e{lbl1 = val}).lbl2` |
| `e{lbl1.lbl2 * val}` | `e{lbl1.lbl2 = e.lbl1.lbl2 * val}` |
| `e{lbl1 = val1, lbl2 = val2}` | `(e{lbl1 = val1}){lbl2 = val2}` |

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

A fuller, more rigorous set of tests are available in the examples directory of [this repository](https://github.com/ndmitchell/record-dot-preprocessor). Those tests take the following considerations into account:

* Basic operations;
* Parametrically polymorphic datatypes;
* Complex record updates including nested updates;
* Multiple occurrences of a field label in a single datatype;
* Infix applications and associativity;
* Interoperability with existing extensions;
* Interoperability with GADTs and existentials.

## Effect and Interactions

**Polymorphic updates:** When enabled, this extension takes the `a{b=c}` syntax and uses it to mean `setField`. The biggest difference a user is likely to experience is that the resulting type of `a{b=c}` is the same as the type `a` - you _cannot_ change the type of the record by updating its fields. The removal of polymorphism is considered essential to preserve decent type inference, and is the only option supported by [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst).

**Stealing a.b syntax:** The `a.b` syntax is commonly used in conjunction with the `lens` library, e.g. `expr^.field1.field2`. Treating `a.b` without spaces as a record projection would break such code. The alternatives would be to use a library with a different lens composition operator (e.g. `optics`), introduce an alias in `lens` for `.` (perhaps `%`), write such expressions with spaces, or not enable this extension when also using lenses. While unfortunate, we consider that people who are heavy users of lens don't feel the problems of inadequate records as strongly, so the problems are lessened.

## Costs and Drawbacks

The implementation of this proposal adds code to the compiler, but not a huge amount. Our [prototype implementation](https://gitlab.haskell.org/shayne-fletcher-da/ghc/commits/record-dot-syntax) shows the parsing changes, which is the most complex part.

If this proposal becomes widely used then it is likely that all Haskell users would have to learn that `a.b` is a record field selection. Fortunately, given how popular this syntax is elsewhere, that is unlikely to surprise new users.

This proposal advocates a different style of writing Haskell records, which is distinct from the existing style. As such, it may lead to the bifurcation of Haskell styles, with some people preferring the lens approach, and some point preferring the syntax presented here. That is no doubt unfortunate, but hard to avoid - `a.b` really is ubiquitous in programming languages. We consider that any solution to the records problem _must_ cause some level of divergence, but note that this mechanism (as distinct from some proposals) localises that divergence in the implementation of a module - users of the module will not know whether the internals used this extension or not.

## Alternatives

The primary alternatives to the problem of records are:

* Using the `lens` library. The concept of lenses is very powerful, but that power can be [complex to use](https://twitter.com/fylwind/status/549342595940237312?lang=en). In many ways lenses let you abstract over record fields, but Haskell has neglected the "unabstracted" case of concrete fields.
* The [`DuplicateRecordFields` extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#duplicate-record-fields) is designed to solve similar problems. Despite the problem being real, I am only able to find two users on Hackage. We evaluated this extension as the basis for DAML, but found it sorely lacking. The rules about what types must be inferred by what point are cumbersome and tricky to work with.
* Some style guidelines mandate that each record should be in a separate module. That works, but then requires qualified modules to access fields - e.g. `Person.name (Company.owner c)`. Forcing the structure of the module system to follow the records also makes circular dependencies vastly more likely, leading to complications such as boot files that are ideally avoided.
* Some style guidelines suggest prefixing each record field with the type name, e.g. `personName (companyOwner c)`. While it works, it isn't pleasant, and many libraries then abbreviate the types to lead to code such as `prsnName (coOwner c)`, which can increase confusion.
* There is a [GHC plugin and preprocessor](https://github.com/ndmitchell/record-dot-preprocessor) that both implement much of this proposal. While both have seen light use, their ergonomics are not ideal. The preprocessor struggles to give good location information given the expansion of substrings. The plugin cannot support the full proposal and leads to error messages mentioning `getField`. Suggesting either a preprocessor or plugin to beginners is not an adequate answer. One of the huge benefits to the `a.b` style in other languages is support for completion in IDE's, which is quite hard given for something not actually in the language.
* Continue to [vent](https://www.reddit.com/r/haskell/comments/vdg55/haskells_record_system_is_a_cruel_joke/) [about](https://bitcheese.net/haskell-sucks) [records](https://medium.com/@snoyjerk/least-favorite-thing-about-haskal-ef8f80f30733) [on](https://www.quora.com/What-are-the-worst-parts-about-using-Haskell) [social](http://www.stephendiehl.com/posts/production.html) [media](https://www.drmaciver.com/2008/02/tell-us-why-your-language-sucks/).

All these approaches are currently used, and represent the "status quo", where Haskell records are considered not fit for purpose.

## Unresolved Questions

There are no unresolved questions at this time.

## Implementation Plan

If accepted, the proposal authors would be delighted to provide an implementation. Implementation depends on the implementation of [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) and [the `NoFieldSelectors` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst).
