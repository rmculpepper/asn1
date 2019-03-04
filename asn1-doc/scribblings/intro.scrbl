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
@racket[IA5String]. (IA5 is the same as ASCII---the 7-bit character
set.)

Racket values are encoded as instances of an ASN.1 type using the
@racket[asn1->bytes/DER] function, which produces a bytestring
(@racket[bytes?]):

@interaction[#:eval the-eval
(asn1->bytes/DER INTEGER 123456)
(asn1->bytes/DER INTEGER (expt 10 30))
(asn1->bytes/DER IA5String "I am the walrus.")
]

A DER bytestring is decoded according to an ASN.1 type to get a Racket
value using the @racket[bytes->asn1/DER] function:

@interaction[#:eval the-eval
(bytes->asn1/DER INTEGER (asn1->bytes/DER INTEGER 123456))
(bytes->asn1/DER INTEGER (asn1->bytes/DER INTEGER (expt 10 30)))
(bytes->asn1/DER IA5String (asn1->bytes/DER IA5String "I am the walrus."))
]

Complex types are created using forms such as @racket[SEQUENCE],
@racket[CHOICE], and @racket[SEQUENCE-OF]. For example, here is an
ASN.1 type for a sequence of integers:

@interaction[#:eval the-eval
(define Integers (SEQUENCE-OF INTEGER))
(asn1->bytes/DER Integers '(1 2 3 -1000))
(bytes->asn1/DER Integers (asn1->bytes/DER Integers '(1 2 3 -1000)))
]

Unlike @racket[SEQUENCE-OF], @racket[SEQUENCE] and @racket[CHOICE]
take multiple components, each labeled with a name and optionally a
tagging directive.

Here is an ASN.1 type representing a three-dimensional point:

@interaction[#:eval the-eval
(define Point (SEQUENCE [x INTEGER] [y INTEGER] [z INTEGER]))
(asn1->bytes/DER Point (hasheq 'x 123 'y 456 'z 789))
]

And here's one representing a reference to a person:

@interaction[#:eval the-eval
(define Person (CHOICE [name IA5String] [number INTEGER]))
(asn1->bytes/DER Person '(name "Jean"))
(asn1->bytes/DER Person '(number 24601))
(bytes->asn1/DER Person (asn1->bytes/DER Person '(number 24601)))
]

Sometimes components of a choice (and sometimes other structured
types) must be given alternative tags because their default tags would
not distinguish between them.

@interaction[#:eval the-eval
(define Employee
  (CHOICE [name  #:implicit 0 IA5String]
          [title #:implicit 1 IA5String]))
(asn1->bytes/DER Employee '(name "Ash"))
(asn1->bytes/DER Employee '(title "Boomstick Specialist"))
]

Attempting to decode an ASN.1 value at a different type than it was
encoded as usually results in an error:

@interaction[#:eval the-eval
(bytes->asn1/DER INTEGER (asn1->bytes/DER IA5String "hello"))
(bytes->asn1/DER Person (asn1->bytes/DER Employee '(name "Ash")))
]

If you don't know the type of an ASN.1 encoding, you can decode it as
the @racket[ANY] type to see the frame structure without interpreting
the primitive contents.

@interaction[#:eval the-eval
(bytes->asn1/DER ANY
  (asn1->bytes/DER Point (hasheq 'x 123 'y 456 'z 789)))
(bytes->asn1/DER ANY
  (asn1->bytes/DER Employee '(title "Boomstick Specialist")))
]
In this example, @racket['universal 16] is the tag for sequences, and
@racket['universal 2] is the tag for integers.

The type @racket[ANY*] is like @racket[ANY], but it additionally
recognizes and translates standard tags:

@interaction[#:eval the-eval
(bytes->asn1/DER ANY*
  (asn1->bytes/DER Point (hasheq 'x 123 'y 456 'z 789)))
(bytes->asn1/DER ANY*
  (asn1->bytes/DER Employee '(title "Boomstick Specialist")))
]

@(close-eval the-eval)
