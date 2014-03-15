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

@section[#:tag "base-types"]{Base Types}

@defthing[BOOLEAN asn1-type?]{

Type of booleans. Corresponds to Racket's @racket[boolean?].
}

@defthing[INTEGER asn1-type?]{

Type of arbitrary-precision, signed integers. Corresponds to Racket's
@racket[exact-integer?].
}

@defthing[BIT-STRING asn1-type?]{

Type of bit strings, including those that end in a partial
octet. Represented by the @racket[bit-string] struct.
}

@defstruct*[bit-string ([bytes bytes?] [unused (integer-in 0 7)])]{

Represents a bit string. The first bit in the bit string is the high
bit of the first octet of @racket[_bytes]. The lowest @racket[_unused]
bits of the last octet of @racket[_bytes] are not considered part of
the bit string; they should be set to 0.
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
         #:grammar ([component [name-id maybe-tag component-type maybe-option]
                               [#:dependent name-id maybe-tag component-type maybe-option]]
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

Represented by Racket values of the following form:

@racketblock[(list 'sequence (list 'name-id _component-value) ...)]

That is, each @racket[_component-value] is labeled with the same
symbol as the corresponding @racket[component] of the type.

If a @racket[component] is specified with the @racket[#:dependent]
keyword, then that @racket[component-type] is not a constant, but is
instead evaluated with the values of preceding fields in scope each
time the type is used for encoding or decoding.

@examples[#:eval the-eval
(define IntOrString
  (Sequence [type-id INTEGER]
            [#:dependent value (get-type type-id)]))
(code:comment "get-type : Integer -> Asn1-Type")
(define (get-type type-id)
  (case type-id
    [(1) INTEGER]
    [(2) IA5String]
    [else (error 'get-type "unknown type-id: ~e" type-id)]))
(DER-encode IntOrString '(sequence [type-id 1] [value 729072]))
(DER-encode IntOrString '(sequence [type-id 2] [value "hello"]))
]

See also @secref["handling-info"].
}

@defform[(Set component ...)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]])
         #:contracts ([component-type asn1-type?])]{

Corresponds the ASN.1 SET type form.
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

@defproc[(Wrap [type asn1-type?]
               [#:pre-encode pre-encode (or/c (-> any/c any/c) #f) #f]
               [#:encode encode (or/c (-> any/c bytes?) #f) #f]
               [#:decode decode (or/c (-> bytes? any/c) #f) #f]
               [#:post-decode post-decode (or/c (-> any/c any/c) #f) #f])
         asn1-type?]{

Produces a type @racket[_wrapped-type] that acts like @racket[type],
but whose encoding is affected by the additional parameters as
follows:

If @racket[pre-encode] is a function, then when encoding @racket[_v]
as @racket[_wrapped-type], evaluate @racket[(pre-encode _v)] and pass
that to the built-in encoding rules (or the next encoder hook) instead
of @racket[_v].

If @racket[encode] is a function, then when encoding @racket[_v] as
@racket[_wrapped-type], evaluate @racket[(encode _v)] and use that as
the value component of the TLV triple instead of calling the built-in
encoding rules.

If @racket[decode] is a function, then when decoding the value
component bytestring @racket[_b] as @racket[_wrapped-type], evaluate
@racket[(decode _b)] and use that as the decoded value instead of
calling the built-in decoding rules.

If @racket[post-decode] is a function, then when decoding a bytestring
as @racket[_wrapped-type], first call the built-in decoding rules (or
the next decoder hook) to get a Racket value @racket[_v], then
evaluate @racket[(post-decode _v)] and return the result as the
decoded value.

One use of wrapped types with encoding rules is for efficiency. For
example, suppose that you already have a large integer in the
appropriate octet-string form (perhaps from another library or from
reading a serialized version). Instead of converting it to a bignum to
pass to @racket[DER-encode], you can add use a wrapped type to accept
the value directly as a bytestring:

@interaction[#:eval the-eval
(define MyInteger
  (Wrap INTEGER #:encode (lambda (b) b)))
(define sig
  (DER-encode (Sequence [r MyInteger] [s MyInteger])
              '(sequence [r #"}nSi|-uy"]
                         [s #"y\21~P#3\37\b"])))
sig
(DER-decode (Sequence [r INTEGER] [s INTEGER]) sig)
]

Beware, no checking is done on the bytestring! In the example above,
the programmer must be sure that the bytestring is an encoding of the
integer as a @emph{big-endian, signed, two's complement base-256
integer repesented using the minimum number of octets}. (A
@hyperlink["https://www.cs.auckland.ac.nz/~pgut001/pubs/x509guide.txt"]{bug
in some X.509 certificate software} was to encode certain numbers
using an @emph{unsigned} encoding.)

Encoding hooks can also be used to add in support for base types not
otherwise supported by this library. See
@secref["handling-unsupported"] for details.

One disadvantage to @racket[Wrap] is that it mixes encoding concerns
into the type structure. See @secref["der-hooks"] for an
alternative. The expression @racket[(Wrap type)] can be used to
generate a type distinct from @racket[type] for the purpose of
targeting encoding or decoding hooks.
}

@defform[(Delay type)
         #:contracts ([type asn1-type?])]{

Produces a type that acts like @racket[type], but delays the
evaluation of @racket[type] until it is needed for encoding or
decoding, or to check the well-formedness of another type.

Use @racket[Delay] to write recursive types or types with forward
references.
}

@defproc[(SequenceOf [component-type asn1-type?])
         asn1-type?]{

Corresponds to the ASN.1 SEQUENCE OF type form.
}

@defproc[(SetOf [component-type asn1-type?])
         asn1-type?]{

Corresponds the the ASN.1 SET OF type form.
}


@section[#:tag "any-type"]{The ANY Type}

@defthing[ANY asn1-type?]{

Unknown or context-dependent type.

There are no built-in encoding rules for @racket[ANY]. The built-in
decoding rules handle the base types listed in @secref["base-types"]
with their normal tags, and they treat all sequence values as
@racket[(SequenceOf ANY)] and all set values as @racket[(SetOf
ANY)]. Non-universal tags cannot be decoded using @racket[ANY].

Unlike all other types, a decoder hook for @racket[ANY] receives the
full TLV triple instead of only the value component, and an encoder
hook for @racket[ANY] must produce a full TLV triple instead of only
the value component.

@interaction[#:eval the-eval
(define ANY-as-bytes (Wrap ANY #:decode (lambda (b) b)))
(define IA5String-as-bytes (Wrap IA5String #:decode (lambda (b) b)))
(DER-encode IA5String "abc")
(DER-decode IA5String-as-bytes (DER-encode IA5String "abc"))
(DER-decode ANY-as-bytes (DER-encode IA5String "abc"))
]
}

@(close-eval the-eval)
