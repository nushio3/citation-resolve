citation-resolve
================

[![Build Status](https://travis-ci.org/nushio3/citation-resolve.png?branch=master)](https://travis-ci.org/nushio3/citation-resolve)


convert document identifiers such as DOI, ISBN, arXiv ID to bibliographic reference.

```
ghci> ref <- resolveDef "arXiv:1204.4779"
ghci> title ref
"Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
ghci> containerTitle ref
"Computational Science and Discovery"
```


This program requrires command-line tool `xsltproc` to parse XML (to resolve ISBN).

cf. http://crosscite.org/cn/ for citation resolve mechanism.
