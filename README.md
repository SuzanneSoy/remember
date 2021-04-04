[![Build Status,](https://img.shields.io/travis/jsmaniac/remember/main.svg)](https://travis-ci.org/jsmaniac/remember)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/remember/main.svg)](https://codecov.io/gh/jsmaniac/remember)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/remember)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/remember/)
[![Maintained as of 2017,](https://img.shields.io/maintenance/yes/2017.svg)](https://github.com/jsmaniac/remember/issues)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

remember
========

This Racket library provides a compile-time memoize feature. It allows
remembering a value with `(remember-write! 'category 'value)`. In subsequent
compilations, `(get-remembered 'category)` will return a set of all
previously-remembered values.

Installation
============

raco pkg install remember

Example use case: the `phc-adt` library
=======================================

This library is used to implement "interned" structure and constructor types
in the [`phc-adt`](https://github.com/jsmaniac/phc-adt) library. The `phc-adt`
library needs to know the set of all structure and constructor types used in
the program, and uses `remember` to automatically memoize structure
descriptors and constructor names.

When the `structure` macro defined in
[`structure.hl.rkt`](https://github.com/jsmaniac/phc-adt/blob/refactor/structure.hl.rkt)
encounters an unknown list of field names, it uses the `remember` library to
append the tuple of field names to a user-specified file. That file is loaded
in subsequent compilations, so that the tuple of fields is known to `phc-adt`.

The memoized descriptors are used to know all possible structs that can
contain a field with the desired name when accessing it with `(get instance
field-name)`. The `get` macro can then retrieve the field's value using the
right accessor (for example `(struct123-fieldname instance)`). Knowing all
existing structures allows `get` to perform some kind of dynamic dispatch to
obtain the appropriate accessor, for example using a `cond` which tests for
all possible types.

The `constructor` macro defined in
[`constructor.hl.rkt`](https://github.com/jsmaniac/phc-adt/blob/refactor/constructor.hl.rkt)
works in the same way, but remembers the name of the constructor's tag instead
of field names. The memoization feature is used so that all uses of a
constructor with a given name are equivalent, across all files.

