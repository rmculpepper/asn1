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

@title[#:tag "der"]{DER Encoding and Decoding}

@defproc[(DER-encode [type asn1-type?] [v any/c])
         bytes?]{

Produces the DER-encoding of @racket[v] as @racket[type].
}

@defproc[(DER-decode [type asn1-type?] [b bytes?])
         any/c]{

Decodes the DER-encoding @racket[b] of @racket[type] to get a Racket
value.
}

@defproc[(DER-read [type asn1-type?] [in input-port?])
         any/c]{

Reads one DER encoding from @racket[in] and decodes it as
@racket[type].
}


@(close-eval the-eval)
