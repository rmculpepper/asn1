#lang scribble/doc
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/match
                     asn1
                     asn1/ber))

@(define the-eval (make-base-eval))
@(the-eval '(require asn1 asn1/ber))

@title[#:tag "codec"]{ASN.1 Encoding and Decoding}

This library supports the Basic Encoding Rules (BER) for ASN.1 as well
as its restricted form, the Distinguished Encoding Rules (DER).

A BER encoding is logically a @emph{tag, length, value} (TLV)
triple. The tag indicates to what type the value belongs (or at least
disambiguates; the tag is often context-specific). The tag also
indicates whether the encoding is @emph{primitive} or
@emph{constructed}---that is, consisting of a sequence of TLV
triples. The length indicates the length of the value encoding, so
even unknown types can be decomposed into TLV triples. The value
component contains a type-specific encoding of the value. For example,
integers are encoded in big-endian, base-256, two's-complement form
using a minimal number of octets.

@defproc[(write-asn1 [type asn1-type?] [value any/c]
                     [out output-port? (current-output-port)]
                     [#:rules rules (or/c 'BER 'DER) 'BER])
         void?]{

Writes to @racket[out] the encoding of @racket[value] as an instance
of the given ASN.1 @racket[type], using the specified encoding
@racket[rules].
}

@defproc[(read-asn1 [type asn1-type?] [in input-port? (current-input-port)]
                    [#:rules rules (or/c 'BER 'DER) 'BER])
         any/c]{

Reads a value of the given ASN.1 @racket[type] from @racket[in], using the
specified encoding @racket[rules].

@history[#:changed "1.2" @elem{Strings and bytes in the result are immutable.}]
}

@defproc[(asn1->bytes [type asn1-type?] [value any/c]
                      [#:rules rules (or/c 'BER 'DER) 'BER])
         bytes?]{

Encodes @racket[value] as an instance of the given ASN.1 @racket[type],
using the specified encoding @racket[rules].
}

@defproc[(bytes->asn1 [type asn1-type?] [bstr bytes?]
                      [#:rules rules (or/c 'BER 'DER) 'BER])
         any/c]{

Decodes @racket[bstr] as an instance of the given ASN.1 @racket[type],
using the specified encoding @racket[rules].

@history[#:changed "1.2" @elem{Strings and bytes in the result are immutable.}]
}

@deftogether[[
@defproc[(write-asn1/DER [type asn1-type?] [value any/c]
                         [out output-port? (current-output-port)])
         void?]
@defproc[(read-asn1/DER [type asn1-type?] [in input-port? (current-input-port)])
         any/c]
@defproc[(asn1->bytes/DER [type asn1-type?] [value any/c]) bytes?]
@defproc[(bytes->asn1/DER [type asn1-type?] [bstr bytes?]) any/c]
]]{

Like @racket[write-asn1], @racket[read-asn1], @racket[asn1->bytes],
and @racket[bytes->asn1], respectively, but use the DER encoding
rules.

Equivalent to calling the corresponding procedure with @racket[#:rules
'DER].

@interaction[#:eval the-eval
(define Integers (SEQUENCE-OF INTEGER))
(asn1->bytes Integers '(1 2 3 -1000))
(asn1->bytes Integers '(1 2 3 -1000) #:rules 'DER)
(asn1->bytes/DER Integers '(1 2 3 -1000))
]

@history[#:changed "1.2" @elem{Strings and bytes in the result are immutable.}]
}


@section[#:tag "ber"]{BER Utilities}

@defmodule[asn1/ber]

@defstruct*[BER-frame 
            ([tag-class (or/c 'universal 'private 'application 'context-sensitive)]
             [tag-number exact-nonnegative-integer?]
             [value (or/c bytes? (listof (or/c bytes? BER-frame?)))])]{

Represents a decomposed TLV triple. The tag is broken down into the
@racket[tagclass] and @racket[tagn] fields. The length is not
represented; it is implicit in the @racket[value] field. The
@racket[value] is a list for ``constructed'' types and a bytestring
for ``primitive'' types.
}


@deftogether[[
@defproc[(BER-encode [type asn1-type?] [v any/c] [#:der? der? boolean? #f])
         BER-frame?]
@defproc[(BER-decode [type asn1-type?] [b BER-frame?] [#:der? der? boolean? #f])
         any/c]
]]{

Encode or decode a value of the given ASN.1 @racket[type] to a BER TLV
frame.

@interaction[#:eval the-eval
(BER-encode INTEGER 5)
(BER-decode INTEGER (BER-frame 'universal 2 #"\5"))
(BER-encode (SEQUENCE-OF INTEGER) '(1 2 3 -1000))
(BER-encode (SEQUENCE [a IA5String] [b INTEGER])
            (hasheq 'a "Jean" 'b 24601))
]
}

@deftogether[[
@defproc[(write-BER-frame [frame BER-frame?]
                          [out output-port? (current-output-port)]
                          [#:rules rules (or/c 'BER 'DER) 'BER])
         void?]
@defproc[(read-BER-frame [in input-port? (current-input-port)]
                         [#:rules rules (or/c 'BER 'DER) 'BER])
         BER-frame?]
]]{

Write or read a BER TLV frame, respectively.
}


@(close-eval the-eval)
