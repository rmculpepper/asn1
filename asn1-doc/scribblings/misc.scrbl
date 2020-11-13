#lang scribble/doc
@(require (for-syntax racket/base)
          scribble/manual
          scribble/basic
          scribble/eval
          (only-in scribble/racket make-element-id-transformer)
          (for-label racket/base
                     racket/contract
                     racket/match
                     asn1 asn1/util/names asn1/util/time))

@(define the-eval (make-base-eval))
@(the-eval '(require asn1 asn1/util/names asn1/util/time))

@title[#:tag "util"]{ASN.1 Miscellaneous Utilities}

@; ----------------------------------------
@section[#:tag "util-bit-string"]{ASN.1 Bit String Utilities}

@defmodule[asn1/util/bit-string]
@history[#:added "1.2"]

@defproc[(bit-string-length [bs bit-string?]) exact-nonnegative-integer?]{

Returns the number of bits in @racket[bs].
}

@defproc[(bit-string-ref [bs bit-string?]
                         [index exact-nonnegative-integer?])
         boolean?]{

Returns the bit stored in @racket[bs] at the given @racket[index] as a
boolean. Note that in ASN.1, the first bit of a bit string is stored
as the @emph{high} bit of the first byte.
}

@; ----------------------------------------
@section[#:tag "util-names"]{ASN.1 Named Value Utilities}

@defmodule[asn1/util/names]
@history[#:added "1.2"]

@defproc[(WRAP-NAMES [type (or/c ENUMERATED BIT-STRING)]
                     [named-values (listof (cons/c symbol? any/c))])
         asn1-type?]{

Wraps @racket[type] with decode and encode hooks to translate to and from named
values.

If @racket[type] is @racket[ENUMERATED], then the corresponding Racket values
are symbols from @racket[named-values] and exact integers for values not in the
list.
@examples[#:eval the-eval
(define xyz (WRAP-NAMES ENUMERATED '((x . 0) (y . 1) (z . 2))))
(bytes->asn1 xyz (asn1->bytes ENUMERATED 2))
(bytes->asn1 xyz (asn1->bytes ENUMERATED 5))
]

If @racket[type] is @racket[BIT-STRING], then the corresponding Racket values
are lists of symbols from @racket[named-values] and exact integers for values
not in the list.
@(let ()
   (the-eval '(define bs1 #b10100000))
   (define-syntax bs1 (make-element-id-transformer (lambda (stx) #'(racketvalfont "#b10100000"))))
   (the-eval '(define bs2 #b10100010))
   (define-syntax bs2 (make-element-id-transformer (lambda (stx) #'(racketvalfont "#b10100010"))))
@examples[#:eval the-eval
(define abc (WRAP-NAMES BIT-STRING '((a . 0) (b . 1) (c . 2))))
(bytes->asn1 abc (asn1->bytes BIT-STRING (bit-string (bytes bs1) 0)))
(bytes->asn1 abc (asn1->bytes BIT-STRING (bit-string (bytes bs2) 0)))
])
}

@; ----------------------------------------
@section[#:tag "util-time"]{ASN.1 Time Utilities}

@defmodule[asn1/util/time]
@history[#:added "1.2"]

@deftogether[[
@defproc[(asn1-utc-time->seconds [s asn1-utc-time?]) rational?]
@defproc[(asn1-generalized-time->seconds [s asn1-generalized-time?]) rational?]
]]{

Interprets @racket[s] as a date and time and returns it as a number of seconds
since midnight UTC, January 1, 1970 (like @racket[current-seconds],
@racket[seconds->date], etc). If @racket[s] omits minutes, seconds, or
fractional seconds, they are treated as 0. If @racket[s] omits the UTC specifier
(@tt{Z}) or timezone offset, the time is interpreted as local time.

The two-digit years of UTCTime are interpreted in the range 1950 to 2049; this
is consistent with X.509 but not all other ASN.1 applications.

@examples[#:eval the-eval
(asn1-utc-time->seconds "20110220-07")
(asn1-generalized-time->seconds "19700101010203Z")
]
}
