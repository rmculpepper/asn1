#lang scribble/manual
@(require scribble/basic
          scribble/eval
          (for-label (except-in racket/base sequence?)
                     racket/contract
                     racket/match
                     asn1
                     asn1/base256
                     asn1/sequence))

@(define the-eval (make-base-eval))
@(the-eval '(require asn1 asn1/base256 asn1/sequence))

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
(base256-normalize-signed (bytes 0 0 0 128))
(base256-normalize-signed (bytes 255 255 1))
(base256-normalize-signed (bytes 255 255 128))
(code:comment "unsigned")
(base256-normalize-unsigned (bytes 0 0 0 1))
(base256-normalize-unsigned (bytes 0 0 0 128))
(base256-normalize-unsigned (bytes 255 255 1))
(base256-normalize-unsigned (bytes 255 255 128))
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

@section[#:tag "sequence-util"]{ASN.1 Sequence Utilities}

@defmodule[asn1/sequence]

The @racketmodname[asn1/sequence] module provides functions useful for
dealing with the S-expression representations of ASN.1 sequence
values. Note: this library is completely unrelated to the Racket
notion of sequence.

@defproc[(sequence? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an S-expression representation of
a @racket[Sequence] value, @racket[#f] otherwise.

@examples[#:eval the-eval
(sequence? '(sequence [a 1] [b 2]))
(sequence? (vector 1 2 3))
]
}

@defproc[(sequence-ref [s sequence?] [key symbol?] [default any/c (lambda () (error ....))])
         any]{

Extracts the field named @racket[key] from the sequence @racket[s]. If
no such field exists, then @racket[default] is called if it is a
procedure, or returned otherwise.

@examples[#:eval the-eval
(sequence-ref '(sequence [a 1] [b 2]) 'a)
(sequence-ref '(sequence [a 1] [b 2]) 'c)
(sequence-ref '(sequence [a 1] [b 2]) 'c 3)
]
}

@(close-eval the-eval)
