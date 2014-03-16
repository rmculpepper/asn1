#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/match
                     asn1
                     asn1/base256))

@(define the-eval (make-base-eval))
@(the-eval '(require asn1 asn1/base256))

@title[#:tag "util"]{ASN.1-Related Utilities}

@section[#:tag "base256"]{Base-256 Integer Representations}

@defmodule[asn1/base256]

@deftogether[[
@defproc[(signed->base256 [n exact-integer?]) bytes?]
@defproc[(base256->signed [b bytes?]) exact-integer?]
]]{

Converts between integers and their big-endian, base-256, two's
complement @emph{signed} encodings---the encoding used by the ASN.1
@racket[INTEGER] type.

@examples[#:eval the-eval
(signed->base256 1)
(signed->base256 255)
(signed->base256 256)
(signed->base256 -1)
(signed->base256 (expt 10 40))
(base256->signed (bytes 127))
(base256->signed (bytes 128))
(base256->signed (bytes 1 1))
]
}

@deftogether[[
@defproc[(unsigned->base256 [n exact-nonnegative-integer?]) bytes?]
@defproc[(base256->unsigned [b bytes?]) exact-nonnegative-integer?]
]]{

Converts between nonnegative integers and their big-endian base-256
@emph{unsigned} encodings.

@examples[#:eval the-eval
(unsigned->base256 1)
(unsigned->base256 255)
(unsigned->base256 256)
(unsigned->base256 (expt 10 40))
(base256->unsigned (bytes 127))
(base256->unsigned (bytes 128))
(base256->unsigned (bytes 1 1))
]
}

@deftogether[[
@defproc[(base256-normalize-signed [b bytes?]) bytes?]
@defproc[(base256-normalize-unsigned [b bytes?]) bytes?]
]]{

Normalizes the given big-endian base-256 signed or unsigned
(respectively) encoding. Unnecessary leading 0 bytes are removed from
nonnegative integers, and unnecessary leading 255 (-1) bytes are
removed from negative integers.

Equivalent to @racket[(signed->base256 (base256->signed b))] and
@racket[(unsigned->base256 (base256->unsigned b))], respectively, but
faster.

@examples[#:eval the-eval
(code:comment "signed")
(base256-normalize-signed (bytes 0 0 0 1))
(base256-normalize-signed (bytes 0 128 0 1))
(base256-normalize-signed (bytes 255 255 128))
(base256-normalize-signed (bytes 255 255 1))
(code:comment "unsigned")
(base256-normalize-unsigned (bytes 0 0 0 1))
(base256-normalize-unsigned (bytes 0 128 0 1))
(base256-normalize-unsigned (bytes 255 255 1))
]
}

@defproc[(base256-unsigned->signed [b bytes?]) bytes?]{

Converts the unsigned encoding of a number to the (normalized) signed
encoding---essentially, adds a leading 0 byte when necessary.

Equivalent to @racket[(signed->base256 (base256->unsigned b))], but
faster.

@examples[#:eval the-eval
(base256-unsigned->signed (bytes 127))
(base256-unsigned->signed (bytes 128))
]
}

@deftogether[[
@defproc[(base256-signed-zero? [b bytes?]) boolean?]
@defproc[(base256-signed-positive? [b bytes?]) boolean?]
@defproc[(base256-signed-negative? [b bytes?]) boolean?]
]]{

Determines whether @racket[b] is the signed encoding of zero, a
positive integer, or a negative integer, respectively.

Equivalent to @racket[(zero? (base256->signed b))], @racket[(positive?
(base256->signed b))], and @racket[(negative? (base256->signed b))],
respectively.
}

@(close-eval the-eval)
