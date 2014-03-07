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

@deftogether[[
@defthing[ANY asn1-type?]
@defthing[BOOLEAN asn1-type?]
@defthing[INTEGER asn1-type?]
@defthing[BIT-STRING asn1-type?]
@defthing[OCTET-STRING asn1-type?]
@defthing[NULL asn1-type?]
@defthing[OBJECT-IDENTIFIER asn1-type?]
@defthing[PrintableString asn1-type?]
@defthing[IA5String asn1-type?]
@defthing[UTF8String asn1-type?]
]]{

Basic ASN.1 types.
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

@defform[(Set component ...)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]])
         #:contracts ([component-type asn1-type?])]{

Corresponds the ASN.1 SET type form.
}

@defform[(SetOf component-type)
         #:contracts ([component-type asn1-type?])]{

Corresponds the the ASN.1 SET OF type form.
}


@(close-eval the-eval)
