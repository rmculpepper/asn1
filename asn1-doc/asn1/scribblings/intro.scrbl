#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/match
                     asn1))

@(define the-eval (make-base-eval))
@(the-eval '(require asn1))

@title[#:tag "intro"]{Introduction to the ASN.1 Library}

This section provides a basic introduction to defining ASN.1 types and
using the types to encode and decode data using DER.

@interaction[#:eval the-eval
(require asn1)
]

ASN.1 has several familiar base types, such as @racket[INTEGER] and
@racket[IA5String]. (IA5 is another term for ASCII---the 7-bit
character set.)

Racket values may be encoded as instances of an ASN.1 type using the
@racket[DER-encode] function, which produces a bytestring
(@racket[bytes?]):

@interaction[#:eval the-eval
(DER-encode INTEGER 123456)
(DER-encode IA5String "I am the walrus.")
]

A DER bytestring can be decoded according to an ASN.1 type to get a
Racket value:

@interaction[#:eval the-eval
(DER-decode INTEGER (DER-encode INTEGER 123456))
(DER-decode IA5String (DER-encode IA5String "I am the walrus."))
]

@;{ Decoding with ANY ...}

You can build more complex types using forms such as
@racket[Sequence], @racket[Choice], and @racket[SequenceOf]. For
example, here is an ASN.1 type for a sequence of integers:

@interaction[#:eval the-eval
(define Integers (SequenceOf INTEGER))
(DER-encode Integers '(1 2 3 -1000 #e1e20))
(DER-decode Integers (DER-encode Integers '(1 2 3 -1000 #e1e20)))
]

Unlike @racket[SequenceOf], @racket[Sequence] and @racket[Choice] take
multiple components, each labeled with a name and optionally a tagging
directive.

Here is an ASN.1 type representing a three-dimensional point:

@interaction[#:eval the-eval
(define Point (Sequence [x INTEGER] [y INTEGER] [z INTEGER]))
(DER-encode Point '(sequence [x 123] [y 456] [z 789]))
]

And here's one representing a reference to a person:

@interaction[#:eval the-eval
(define Person (Choice [name IA5String] [number INTEGER]))
(DER-encode Person '(name "Jean"))
(DER-encode Person '(number 24601))
]

Sometimes components of a Choice (and sometimes other structured
types) must be given alternative tags because their default tags would
not distinguish between them.

@interaction[#:eval the-eval
(define DirectoryEntry
  (Choice [name  #:implicit 0 IA5String]
          [title #:implicit 1 IA5String]))
(DER-encode DirectoryEntry '(name "Ash"))
(DER-encode DirectoryEntry '(title "Boomstick Specialist"))
]


@(close-eval the-eval)
