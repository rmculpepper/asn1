# ASN.1 Parser

This directory contains an **incomplete** parser for ASN.1 modules with a
translator to Racket's `asn1` library.

Note: This directory is not a package.

Example: `racket translate.rkt examples/pkcs1.asn`

## Dependencies

This library depends on `asn1-lib` and the experimental parser library at
https://github.com/rmculpepper/parsing.

## Known Bugs and Limitations

The syntax of ASN.1 modules (as opposed to the binary formats *defined* by ASN.1
modules) is obnoxiously ambiguous and difficult to parse. The translator
attempts to disambiguate definitions by a rudimentary form of type-checking, but
it doesn't always succeed.

Other issues with the translator:
- It generates "FIXME" comments in some cases when it cannot emit reasonable code.
- It discards constraints (for example, `SIZE` constraints).
- It does not generate real code for Information Class definitions and for types
  etc that are parameterized by them.

In short, the translator tries to generate something that's in the right
ballpark, but the output requires manual inspection and editing before it is
usable.
