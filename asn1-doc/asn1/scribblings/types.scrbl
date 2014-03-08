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

@title[#:tag "types"]{ASN.1 Types}

@defproc[(asn1-type? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is an ASN.1 type, @racket[#f]
otherwise.
}

@section{Base Types}

@defthing[ANY asn1-type?]{

Unknown or context-dependent type.
}

@defthing[BOOLEAN asn1-type?]{

Type of booleans. Corresponds to Racket's @racket[boolean?].
}

@defthing[INTEGER asn1-type?]{

Type of arbitrary-precision, signed integers. Corresponds to Racket's
@racket[exact-integer?].
}

@defthing[BIT-STRING asn1-type?]{

Type of bit strings, including those that end in a partial octet.

@;{FIXME: support partial-octet bit strings ...}
}

@defthing[OCTET-STRING asn1-type?]{

Type of octet strings. Corresponds to Racket's @racket[bytes?].
}

@defthing[NULL asn1-type?]{

Indicates no information. Represented by the Racket value @racket[#f].
}

@defthing[OBJECT-IDENTIFIER asn1-type?]{

Type of references to ``objects'' in a hierarchical
registry. Represented by Racket @racket[(listof
exact-nonnegative-integer?)] of length at least 2, where the first
integer is between 0 and 2 (inclusive) and the second is between 0 and
39 (inclusive). There is no upper bound on subsequent integers.

@examples[
(define rsadsi '(1 2 840 113549))
(define pkcs1 (append rsadsi '(1 1)))
]
}

@defthing[PrintableString asn1-type?]{

Subset of ASCII strings containing only the ``printable'' characters,
which consist of @litchar{A} to @litchar{Z}, @litchar{a} to
@litchar{z}, @litchar{0} to @litchar{9}, the space character, and the
characters in @litchar{'()+,-./:=?}.

Represented by Racket strings satisfying the
@racket[printable-string?] predicate.
}

@defproc[(printable-string? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string containing only the
characters allowed by @racket[PrintableString], @racket[#f] otherwise.
}

@defthing[IA5String asn1-type?]{

Type of ASCII string (IA5 is ASCII). Note that ASCII/IA5 consists of
only 7-bit characters; it is not the same as Latin-1.

Represented by Racket strings satisfying the @racket[ia5string?]
predicate.
}

@defproc[(ia5string? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string containing only the
characters allowed by @racket[IA5String] (that is, characters 0
through 127), @racket[#f] otherwise.
}

@defthing[UTF8String asn1-type?]{

Type of Unicode strings encoded using UTF-8. Corresponds to Racket's
@racket[string?].
}

@section{Structured Types}

@defform[(Sequence component ...)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]]
                    [maybe-tag (code:line)
                               (code:line maybe-tag-class #:implicit tag-number)
                               (code:line maybe-tag-class #:explicit tag-number)]
                    [maybe-tag-class (code:line)
                                     (code:line #:universal)
                                     (code:line #:application)
                                     (code:line #:private)]
                    [maybe-option (code:line)
                                  (code:line #:optional)
                                  (code:line #:default default-expr)])
         #:contracts ([component-type asn1-type?])]{

Corresponds to the ASN.1 SEQUENCE type form.
}

@defform[(SequenceOf component-type)
         #:contracts ([component-type asn1-type?])]{

Corresponds to the ASN.1 SEQUENCE OF type form.
}

@defform[(Choice alternative ...)
         #:grammar ([alternative [name-id maybe-tag alternative-type maybe-option]])
         #:contracts ([alternative-type asn1-type?])]{

Corresonds to the ASN.1 CHOICE type form.
}

@defform[(Tag maybe-tag-class tag type)
         #:grammar ([tag (code:line #:implicit tag-number)
                         (code:line #:explicit tag-number)])
         #:contracts ([type asn1-type?])]{

Corresponds to an ASN.1 alternatively tagged type.
}

@defform[(Set component ...)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]])
         #:contracts ([component-type asn1-type?])]{

Corresponds the ASN.1 SET type form.
}

@defform[(SetOf component-type)
         #:contracts ([component-type asn1-type?])]{

Corresponds the the ASN.1 SET OF type form.
}


@section{Handling Unsupported Types}

ASN.1 defines many additional base types that are unsupported by this
library. An example is T61String, which has escape codes for changing
the interpretation of following characters. It is infeasible for this
library to handle the validation and interpretation of T61String, so
it does not define the type at all. However, an application may define
the type (or an approximation, if full validation and interpretation
are not needed) using @racket[Tag] or @racket[Choice] with a universal
implicit tag.

Here is a basic definition of @racket[T61String] using @racket[Tag]:

@interaction[#:eval the-eval
(define T61String (Tag #:universal #:implicit 20 OCTET-STRING))
(DER-encode OCTET-STRING #"abc")
(code:line (DER-encode T61String #"abc") (code:comment "note different tag byte"))
(DER-decode T61String (DER-encode T61String #"abc"))
]

When encoding a @racket[T61String], the same Racket values are
accepted as for @racket[OCTET-STRING]---that is, bytestrings
(@racket[bytes?])---and the same validation is performed---that is,
none. Likewise when decoding. Additional validation and interpretation
can be attached to the type via @secref["der-hooks"].

An alternative is a @racket[Choice] type with a single variant. The
effect is the same except for the symbolic label on the Racket values.

@interaction[#:eval the-eval
(define T61String*
  (Choice [t61string #:universal #:implicit 20 OCTET-STRING]))
(DER-encode T61String* '(t61string #"abc"))
(DER-decode T61String* (DER-encode T61String* '(t61string #"abc")))
]


@(close-eval the-eval)
