[![Build Status,](https://img.shields.io/travis/jsmaniac/subtemplate/main.svg)](https://travis-ci.org/jsmaniac/subtemplate)
[![Build Stats,](https://img.shields.io/website-stats-stats%20unavailable-blue-red/http/jsmaniac.github.io/travis-stats/.svg?label=build)](http://jsmaniac.github.io/travis-stats/#jsmaniac/subtemplate)
[![Online Documentation,](https://img.shields.io/website-online-offline-blue-red/http/docs.racket-lang.org/subtemplate/.svg?label=docs)](http://docs.racket-lang.org/subtemplate/)
[![Maintained as of 2018,](https://img.shields.io/maintenance/yes/2018.svg)](https://github.com/jsmaniac/subtemplate/issues)
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