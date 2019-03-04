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

@defform[(define-asn1-type name-id type-expr)
         #:contracts ([type-expr asn1-type?])]{

Equivalent to
@racketblock[(define name-id (DELAY type-expr))]
Useful for defining types with forward references. See also
@secref["handling-defs"].
}


@section[#:tag "base-types"]{Base Types}

@defthing[BOOLEAN asn1-type?]{

Type of booleans. Corresponds to Racket's @racket[boolean?].
}

@defthing[INTEGER asn1-type?]{

Type of arbitrary-precision, signed integers. Corresponds to Racket's
@racket[exact-integer?].
}

@defthing[ENUMERATED asn1-type?]{

Type of enumerations. Corresponds to Racket's @racket[exact-integer?].
}

@defthing[BIT-STRING asn1-type?]{

Type of bit strings, including those that end in a partial
octet. Represented by the @racket[bit-string] struct.
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

See also @racket[OID] and @racket[build-OID].
}

@defthing[RELATIVE-OID asn1-type?]{

Type of relative object identifiers. Represented by Racket
@racket[(listof exact-nonnegative-integer?)].
}

@defthing[PrintableString asn1-type?]{

Subset of ASCII strings containing only the ``printable'' characters,
which consist of @litchar{A} to @litchar{Z}, @litchar{a} to
@litchar{z}, @litchar{0} to @litchar{9}, the space character, and the
characters in @litchar{'()+,-./:=?}. (Note that some ASCII characters
normally considered printable, such as @litchar["@"] and @litchar{_}, do
not fit within ASN.1's notion of printable.)

Represented by Racket strings satisfying the
@racket[asn1-printable-string?] predicate.
}

@defthing[IA5String asn1-type?]{

Type of ASCII string (IA5 is ASCII). Note that ASCII/IA5 consists of
only 7-bit characters; it is not the same as Latin-1.

Represented by Racket strings satisfying the @racket[ascii-string?]
predicate.
}

@defthing[UTF8String asn1-type?]{

Type of Unicode strings encoded using UTF-8. Corresponds to Racket's
@racket[string?].
}

@section{Structured Types}

@defform[(SEQUENCE component ... maybe-extensible)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]
                               [name-id maybe-tag #:dependent component-type maybe-option]]
                    [maybe-tag (code:line)
                               (code:line maybe-tag-class #:implicit tag-number)
                               (code:line maybe-tag-class #:explicit tag-number)]
                    [maybe-tag-class (code:line)
                                     (code:line #:universal)
                                     (code:line #:application)
                                     (code:line #:private)]
                    [maybe-option (code:line)
                                  (code:line #:optional)
                                  (code:line #:default default-expr)]
                    [maybe-extensible (code:line)
                                      (code:line #:extensible extension-id)])
         #:contracts ([component-type asn1-type?])]{

Corresponds to the ASN.1 @tt{SEQUENCE} type form.

Represented by Racket hashes of the following form:
@racketblock[(hasheq 'name-id _component-value ... ...)]
That is, the hash maps component-name symbols to the corresponding
@racket[component-type] field values.

If a @racket[component-type] is preceded with the @racket[#:dependent]
keyword, then that @racket[component-type] is not constant, but is
instead evaluated with the values of preceding fields in scope each
time the type is used for encoding or decoding.

@examples[#:eval the-eval
(define IntOrString
  (SEQUENCE [type-id INTEGER]
            [value #:dependent (get-type type-id)]))
(code:comment "get-type : Integer -> Asn1-Type")
(define (get-type type-id)
  (case type-id
    [(1) INTEGER]
    [(2) IA5String]
    [else (error 'get-type "unknown type-id: ~e" type-id)]))
(asn1->bytes/DER IntOrString (hasheq 'type-id 1 'value 729072))
(asn1->bytes/DER IntOrString (hasheq 'type-id 2 'value "hello"))
]

If @racket[#:extensible extension-id] is specified after the
components, then the sequence type is extensible, and any extra final
components found when parsing an instance of the type are included in
the result hash under the key @racket['extension-id]---that is,
@racket['extension-id] is mapped to a non-empty list of
@racket[BER-frame]s. If extra components are found when parsing an
instance of a non-extensible sequence type, an exception is raised.

See also @secref["handling-info"].

@history[#:changed "1.1" @elem{Added the @racket[#:extensible] option.}]
}

@defform[(SET component ... maybe-extensible)
         #:grammar ([component [name-id maybe-tag component-type maybe-option]]
                    [maybe-extensible (code:line)
                                      (code:line #:extensible extension-id)])
         #:contracts ([component-type asn1-type?])]{

Corresponds the ASN.1 @tt{SET} type form.

Represented by Racket values of the following form:
@racketblock[(hash 'name-id _component-value ... ...)]
where each @racket[_component-value] is a @racket[component-type] value.

The syntax for components and extensibility are similar to that of
@racket[SEQUENCE], except that component types cannot depend on other
component values.

@history[#:changed "1.1" @elem{Added the @racket[#:extensible] option.}]
}

@defproc[(SEQUENCE-OF [component-type asn1-type?])
         asn1-type?]{

Corresponds to the ASN.1 @tt{SEQUENCE OF} type form.

Represented by Racket values of the following form:
@racketblock[(list _component-value ...)]
where each @racket[_component-value] is a @racket[component-type] value.
}

@defproc[(SET-OF [component-type asn1-type?])
         asn1-type?]{

Corresponds the the ASN.1 @tt{SET OF} type form.

Represented by Racket values of the following form:
@racketblock[(list _component-value ...)]
where each @racket[_component-value] is a @racket[component-type] value.
}

@defform[(CHOICE variant ... maybe-extensible)
         #:grammar ([variant [name-id maybe-tag variant-type maybe-option]]
                    [maybe-extensible (code:line)
                                      (code:line #:extensible extension-id)])
         #:contracts ([variant-type asn1-type?])]{

Corresonds to the ASN.1 @tt{CHOICE} type form.

Represented by Racket values of the following form:
@racketblock[(list _variant-name-symbol _variant-value)]
where @racket[_variant-value] is a value of the @racket[variant-type] in the
variant named by @racket[_variant-name-symbol].

If @racket[#:extensible extension-id] is specified after the variants,
then the choice type is extensible, and any unknown tag found when
parsing an instance of the type is parsed as @racket[ANY] and labeled
with @racket['extension-id]. If an unknown tag is found when parsing
an instance of a non-extensible choice type, an exception is raised.

@history[#:changed "1.1" @elem{Added the @racket[#:extensible] option.}]
}

@defform[(TAG maybe-tag-class tag type)
         #:grammar ([tag (code:line #:implicit tag-number)
                         (code:line #:explicit tag-number)])
         #:contracts ([type asn1-type?])]{

Corresponds to an ASN.1 alternatively tagged type.

The representation is the same as that of @racket[type].
}

@defproc[(WRAP [type asn1-type?]
               [#:encode encode (or/c (-> any/c any/c) #f) #f]
               [#:decode decode (or/c (-> any/c any/c) #f) #f])
         asn1-type?]{

Produces a type @racket[_wrapped-type] that acts like @racket[type],
but whose encoding is affected by the additional parameters as
follows:

If @racket[encode] is a function, then when encoding @racket[_v] as
@racket[_wrapped-type], evaluate @racket[(encode _v)] and pass that to
the built-in encoding rules (or the next encoder hook) instead of
@racket[_v].

If @racket[decode] is a function, then when decoding a bytestring as
@racket[_wrapped-type], first call the built-in decoding rules (or the
next decoder hook) to get a Racket value @racket[_v], then evaluate
@racket[(decode _v)] and return the result as the decoded value.

Encoding hooks can also be used to add in support for base types not
otherwise supported by this library. See
@secref["handling-unsupported"] for details.
}

@defform[(DELAY type)
         #:contracts ([type asn1-type?])]{

Produces a type that acts like @racket[type], but delays the
evaluation of @racket[type] until it is needed for encoding or
decoding, or to check the well-formedness of another type.

Use @racket[DELAY] to write recursive types or type definitions with
forward references.
}


@section[#:tag "any-type"]{The ANY Type}

@defthing[ANY asn1-type?]{

Unknown or context-dependent type. Represented as a
@racket[BER-frame]. Decoding an unknown ASN.1 encoding as @racket[ANY]
shows the frame structure and tags, and may be help you deduce what
the encoding represents.

@interaction[#:eval the-eval
(bytes->asn1 ANY
  (asn1->bytes (SEQUENCE-OF INTEGER) '(1 2 3 -1000)))
(bytes->asn1 ANY
  (asn1->bytes (SEQUENCE [a IA5String] [b INTEGER])
               (hasheq 'a "Jean" 'b 24601)))
]
}

@defthing[ANY* asn1-type?]{

Like @racket[ANY], but additionally recognizes and translates standard
universal tags.

@history[#:added "1.1"]}


@section[#:tag "type-util"]{ASN.1 Type Utilities}

@defstruct*[bit-string ([bytes bytes?] [unused (integer-in 0 7)])]{

Represents a bit string. The first bit in the bit string is the high
bit of the first octet of @racket[_bytes]. The lowest @racket[_unused]
bits of the last octet of @racket[_bytes] are not considered part of
the bit string; they should be set to 0.
}

@defform[(OID oid-component ...)
         #:grammar ([oid-component arc-nat
                                   (arc-name-id arc-nat)])]{

Notation for object identifiers that allows named arcs. The resulting
value consists only of the arc numbers, however.

@examples[#:eval the-eval
(OID (iso 1) (member-body 2) (us 840) (rsadsi 113549))
(OID (iso 1) (member-body 2) (us 840) (rsadsi 113549) (pkcs 1) 1)
]
}

@defform[(build-OID oid-expr oid-component ...)
         #:grammar ([oid-component arc-nat
                                   (arc-name-id arc-nat)])
         #:contracts ([oid-expr (listof exact-nonnegative-integer?)])]{

Notation for object identifiers that extend existing object
identifiers.

@examples[#:eval the-eval
(define rsadsi (OID (iso 1) (member-body 2) (us 840) (rsadsi 113549)))
(define pkcs-1 (build-OID rsadsi (pkcs 1) 1))
pkcs-1
]
}

@defproc[(asn1-printable-string? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string containing only the
characters allowed by @racket[PrintableString], @racket[#f] otherwise.
}

@defproc[(ascii-string? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string containing only the
characters allowed by @racket[IA5String] (that is, characters 0
through 127), @racket[#f] otherwise.
}


@(close-eval the-eval)
