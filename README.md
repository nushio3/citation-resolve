citation-resolve
================

convert document identifiers such as DOI, ISBN, arXiv ID to bibliographic reference.

```
>>>  ref <- forceEither <$> readArXiv "1204.4779"
>>> title ref
"Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
>>> containerTitle ref
"Computational Science and Discovery"
```
