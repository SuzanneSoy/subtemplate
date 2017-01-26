[![Build Status,](https://img.shields.io/travis/jsmaniac/subtemplate/master.svg)](https://travis-ci.org/jsmaniac/subtemplate)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/subtemplate/master.svg)](https://coveralls.io/github/jsmaniac/subtemplate)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/subtemplate)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/subtemplate/)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)

subtemplate
===========

Automatic generation of temporary identifiers for racket syntax templates,
based on subscripts indices on the identifiers (e.g. generate `yᵢ …` from `xᵢ
…`).

Note that the syntax pattern variables must be matched with one of the patched
forms from `stxparse-info/parse` or `stxparse-info/case`, instead of the
syntax pattern-matching forms from `syntax/parse` or `racket/base,
respectively.