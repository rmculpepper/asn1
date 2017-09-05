#lang scribble/manual
@(require scribble/basic
          scribble/eval
          (for-label (except-in racket/base sequence?)
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

@(close-eval the-eval)
