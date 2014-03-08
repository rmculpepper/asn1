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

One common encoding (or ``transfer syntax'') for ASN.1 types is called
DER (for ``distinguished encoding rules''). It is a restricted form of
BER (``basic encoding rules'').

A DER-encoded value is logically a @emph{tag, length, value} (TLV)
triple. The tag indicates to what type the value belongs (or at least
disambiguates; the tag is often context-specific). The tag also
indicates whether the encoding is @emph{primitive} or
@emph{constructed}---that is, consisting of a sequence of TLV
triples. The length indicates the length of the value encoding, so
even unknown types can be decomposed into TLV triples. The value
component contains a type-specific encoding of the value. For example,
integers are encoded in big-endian, base-256, two's-complement form
using a minimal number of octets.

@defproc[(DER-encode [type asn1-type?] [v any/c])
         bytes?]{

Encodes @racket[v] as ASN.1 type @racket[type] using DER, producing a
TLV triple.
}

@defproc[(DER-decode [type asn1-type?] [b bytes?])
         any/c]{

Decodes the DER TLV triple @racket[b] as ASN.1 type @racket[type],
producing a Racket value.
}

@defproc[(DER-read [type asn1-type?] [in input-port?])
         any/c]{

Reads one DER TLV triple from @racket[in] and decodes it as
@racket[type], producing a Racket value.
}


@section{Low-level DER Operations}

@defstruct*[DER-frame 
            ([tagclass (or/c 'universal 'private 'application 'context-sensitive)]
             [p/c (or/c 'primitive 'constructed)]
             [tagnum exact-nonnegative-integer?]
             [value bytes?])]{

Represents a decomposed TLV triple. The tag is broken down into the
@racket[tagclass], @racket[p/c], and @racket[tagnum] fields. The
length is not represented; it is computed from the length of the
@racket[value] field.
}

@deftogether[[
@defproc[(DER-frame->bytes [frame DER-frame?]) bytes?]
@defproc[(bytes->DER-frame [b bytes?]) DER-frame?]
@defproc[(read-DER-frame [in input-port?]) DER-frame?]
]]{

Encode, decode, or read a DER frame, respectively.

@examples[#:eval the-eval
(bytes->DER-frame (DER-encode INTEGER 123456))
]
}

@defproc[(DER-encode-value [type asn1-type?] [v any/c])
         bytes?]{

Encodes @racket[v] as ASN.1 type @racket[type] using DER, producing a
raw value component instead of the full TLV triple.

@examples[#:eval the-eval
(DER-encode-value INTEGER 123456)
(bytes->DER-frame (DER-encode INTEGER 123456))
]
}

@defproc[(DER-decode-value [type asn1-type?] [b bytes?])
         any/c]{

Decodes the raw value component @racket[b] as ASN.1 type
@racket[type], producing a Racket value.
}

@section[#:tag "der-hooks"]{DER Encoding and Decoding Hooks}

Encoding and decoding can be modified through type-indexed hooks,
which control the encoding and decoding of the @emph{value} components
of the corresponding types. (Hooks cannot influence the handling of
tag or length components---except in the special case of @racket[ANY]
type; see below.)

@defparam[DER-encode-hooks hooks
          (listof (list/c asn1-type? 'pre (-> any/c bytes?)))]{

Parameter of hooks for controlling encoding. The hook

@racketblock[(list _type 'pre _encoder)]

is interpreted as follows: When encoding a Racket value @racket[_v] as
ASN.1 type @racket[type], use the result of @racket[(_encoder _v)]
instead of the built-in encoding rules for that type.

Encoding hooks can be used for efficiency. For example, suppose that
you already have a large integer in the appropriate octet-string form
(perhaps from another library or from reading a serialized
version). Instead of converting it to a bignum to pass to
@racket[DER-encode], you can add a hook to accept the value directly
as a bytestring:

@interaction[#:eval the-eval
(define sig
  (parameterize ((DER-encode-hooks
                  (list (list INTEGER 'pre (lambda (b) b)))))
    (DER-encode (Sequence [r INTEGER] [s INTEGER])
                '(sequence [r #"}nSi|-uy"]
                           [s #"y\21~P#3\37\b"]))))
sig
(DER-decode (Sequence [r INTEGER] [s INTEGER]) sig)
]

Beware, no checking is done on the value! In the example above, the
programmer must be sure that the bytestring is an encoding of the
integer as a @emph{big-endian, signed, two's complement base-256
integer repesented using the minimum number of octets}. (A
@hyperlink["https://www.cs.auckland.ac.nz/~pgut001/pubs/x509guide.txt"]{bug
in some X.509 certificate software} was to encode certain numbers
using an @emph{unsigned} encoding.)

Encoding hooks can also be used to add in support for base types not
otherwise supported by this library. For example, the ASN.1 type
T61String could be defined and used as follows:

@interaction[#:eval the-eval
(define T61String (Tag #:universal #:implicit 20 OCTET-STRING))
(parameterize ((DER-encode-hooks
                (list (list T61String 'pre string->bytes/latin-1))))
  (DER-encode T61String "looks like ASCII to me"))
]
}

@defparam[DER-decode-hooks hooks
          (listof
           (or/c
            (list/c asn1-type? 'pre (-> bytes? any/c))
            (list/c asn1-type? 'post (-> any/c any/c))))]{

Parameter of hooks for controlling decoding. There are two kinds of
hooks, distinguished by whether they are run before or after the
built-in decoding rules:

@itemlist[

@item{@racket[(list _type 'pre _decoder)] ---
When decoding a bytestring @racket[_b] (the value component of a DER
encoding) as ASN.1 type @racket[_type], use the result of
@racket[(_decoder _b)] instead of the built-in decoding rules
for that type.}

@item{@racket[(list _type 'post _decoder)] ---
is interpreted as follows: After decoding a bytestring as ASN.1 type
@racket[_type] to a Racket value @racket[_v] using the built-in decoding
rules, evaluate @racket[(_decoder _v)] and return the result as
the decoded value.}
]

If @racket[(DER-decode-hooks)] contains both a @racket['pre] hook and
a @racket['post] hook for a given type, only the @racket['pre] hook is
used.
}

@(close-eval the-eval)
