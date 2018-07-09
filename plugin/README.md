# record-dot-plugin

A GHC plugin version of `record-dot-preprocessor`. Only works with GHC HEAD. It involves a combination of:

* [Source plugins](https://github.com/nboldi/ghc-proposals/blob/patch-4/proposal-source-plugins.rst), to transform `a.b` into `getField @"b" a`. The code was adapted from [this example](http://oleg.fi/gists/posts/2018-07-06-idiom-brackets-via-source-pluging.html).
* [Overloaded records](https://github.com/adamgundry/ghc-proposals/blob/overloaded-record-fields/proposals/0000-overloaded-record-fields.rst) and [the associated wiki page](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields), which provide `GHC.Records` and a `getField` method that is handled specially by the type-checker.
