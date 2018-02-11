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

@title[#:tag "translating"]{Translating Existing ASN.1 Definitions}

This section gives general advice on translating existing ASN.1
modules. 

For additional illustration, the @tt{examples} directory contains
examples of a few ASN.1 modules from internet standards translated to
use this library. Use
@racketblock[(collection-path "asn1" "examples")] 
to locate the local directory containing the examples, or
@hyperlink["https://github.com/rmculpepper/asn1/tree/master/asn1/examples/"]{view
them online}.

@section[#:tag "handling-defs"]{Translating ASN.1 Definition Order}

In ASN.1 modules, definition order doesn't matter, and ASN.1 code
often takes advantage of this by ordering definitions
top-down. Translating the ASN.1 module to Racket may require
reordering the definitions.

For complex modules, it can be difficult to find a legal ordering that
also preserves relatedness. In such cases, it may be better to split
the module into three coarse sections, consisting of object identifier
definitions, type definitions, and information object set definitions.

Object identifier definitions should come first, because they never
depend on types or information object sets, and they are easy to
locally reorder. Use @racket[OID] and @racket[build-OID] to preserve
the mnemonic names associated with arc numbers.

Type definitions should come next. Type definitions can be made
independent of ordering by defining them using
@racket[define-asn1-type], which is a simple abbreviation for
@racket[define] with @racket[Delay]. Alternatively, type definitions
can be locally reordered bottom-up. Associated value definitions, such
as named integers, can be left close to their associated types, or
they can be moved to a different section.

Information object set definitions should come last, because they
typically refer to both object identifiers and types. Occasionally
they must be reordered within their section, for example when an
information object set aggregates other sets.


@section[#:tag "handling-info"]{Translating Information Classes, Objects, and Object Sets}

This library does not directly support ASN.1 information classes,
objects, and object sets. In general, however, only information object
sets need to be represented, and they can be represented as functions,
dictionaries, or association lists.

Consider the following ASN.1 code from @cite["PKCS1"]:

@verbatim|<<{
ALGORITHM-IDENTIFIER ::= CLASS {
    &id    OBJECT IDENTIFIER  UNIQUE,
    &Type  OPTIONAL
} WITH SYNTAX { OID &id [PARAMETERS &Type] }

-- Allowed EME-OAEP and EMSA-PSS digest algorithms.
OAEP-PSSDigestAlgorithms ALGORITHM-IDENTIFIER ::= {
    { OID id-sha1 PARAMETERS NULL   }|
    { OID id-sha256 PARAMETERS NULL }|
    { OID id-sha384 PARAMETERS NULL }|
    { OID id-sha512 PARAMETERS NULL },
    ...  -- Allows for future expansion -- }
}>>|

The definition for @tt{ALGORITHM-IDENTIFIER} indicates that the class
serves as a mapping from @tt{OBJECT IDENTIFIER}s to types. The
following information object set can be represented as an association
list as follows:

@racketblock[
(define OAEP-PSSDigestAlgorithms
  (list (list id-sha1   NULL)
        (list id-sha256 NULL)
        (list id-sha384 NULL)
        (list id-sha512 NULL)))
]

The following ASN.1 code defines a type parameterized by an
@tt{ALGORITHM IDENTIFIER} information object set, and then
instantiates the type at the set defined above:

@verbatim|<<{
AlgorithmIdentifier { ALGORITHM-IDENTIFIER:InfoObjectSet } ::= SEQUENCE {
    algorithm  ALGORITHM-IDENTIFIER.&id({InfoObjectSet}),
    parameters ALGORITHM-IDENTIFIER.&Type({InfoObjectSet}{@.algorithm}) 
                 OPTIONAL }

HashAlgorithm ::= AlgorithmIdentifier { {OAEP-PSSDigestAlgorithms} }
}>>|

Type parameterization can be represented as a Racket function, and
instantiation as application. The type lookup can be accomplished by a
dependent sequence field type.

@racketblock[
(define (AlgorithmIdentifier InfoObjectSet)
  (SEQUENCE [algorithm OBJECT-IDENTIFIER]
            [parameters #:dependent (cadr (assoc algorithm InfoObjectSet))]))
(define HashAlgorithm (AlgorithmIdentifier OAEP-PSSDigestAlgorithms))
]


@section[#:tag "handling-unsupported"]{Handling Unsupported Types}

ASN.1 defines many additional base types that are unsupported by this
library. An example is T61String, which has escape codes for changing
the interpretation of following characters. It is infeasible for this
library to handle the validation and interpretation of T61String, so
it does not define the type at all. However, an application may define
the type (or an approximation, if full validation and interpretation
are not needed) using @racket[Tag] with a universal implicit tag.

Here is a basic definition of @racket[T61String] using @racket[Tag]:

@interaction[#:eval the-eval
(define T61String (TAG #:implicit #:universal 20 OCTET-STRING))
(asn1->bytes/DER OCTET-STRING #"abc")
(code:line (asn1->bytes/DER T61String #"abc") (code:comment "note different tag byte"))
(bytes->asn1/DER T61String (asn1->bytes/DER T61String #"abc"))
]

When encoding a @racket[T61String], the same Racket values are
accepted as for @racket[OCTET-STRING]---that is, bytestrings
(@racket[bytes?])---and the same validation is performed---that is,
none. Likewise when decoding. 

To change the way T61Strings are encoded and decoded, use
@racket[WRAP] to add encoding and decoding rules. Let us pretend for a
moment that T61Strings are just Latin-1 strings. Then we could define
T61String with automatic conversion to and from Racket strings as
follows:

@interaction[#:eval the-eval
(define T61String
  (WRAP (TAG #:implicit #:universal 20 OCTET-STRING)
        #:encode string->bytes/latin-1
        #:decode bytes->string/latin-1))
(asn1->bytes/DER T61String "pretend T61 is Latin-1")
(bytes->asn1/DER T61String (asn1->bytes/DER T61String "pretend T61 is Latin-1"))
]

The following ASN.1 pseudo-definitions may be helpful in translating
ASN.1 features not directly supported by this library. (Page numbers
refer to @cite["Dubuisson2001"].)

@verbatim|>>{
-- pp141-145: REAL

REAL ::= [UNIVERSAL 9] IMPLICIT -- complicated --

-- p175: Additional string types

NumericString   ::= [UNIVERSAL 18] IMPLICIT OCTET STRING
VisibleString   ::= [UNIVERSAL 19] IMPLICIT OCTET STRING
ISO646String    ::= VisibleString
TeletexString   ::= [UNIVERSAL 20] IMPLICIT OCTET STRING
T61String       ::= TeletexString
VideotexString  ::= [UNIVERSAL 21] IMPLICIT OCTET STRING
GraphicString   ::= [UNIVERSAL 25] IMPLICIT OCTET STRING
GeneralString   ::= [UNIVERSAL 27] IMPLICIT ...
UniversalString ::= [UNIVERSAL 28] IMPLICIT OCTET STRING -- UCS-4
BMPString       ::= [UNIVERSAL 30] IMPLICIT OCTET STRING -- UCS-2

-- pp198-199: Object descriptors

ObjectDescriptor ::= [UNIVERSAL 7] IMPLICIT GraphicString

-- pp199-204: Time types

GeneralizedTime ::= [UNIVERSAL 24] IMPLICIT OCTET STRING
  -- YYYYMMDDhh[mm[ss[.f+]]]Z

UTCTime         ::= [UNIVERSAL 23] IMPLICIT OCTET STRING
  -- YYMMDDhhmm[ss]Z
  -- Note 2-digit year!

-- p301: EXTERNAL

EXTERNAL ::= [UNIVERSAL 8] IMPLICIT SEQUENCE {
  -- at least one of {direct-reference, indirect-reference} is required
  direct-reference      OBJECT IDENTIFIER OPTIONAL,
  indirect-reference    INTEGER OPTIONAL,
  data-value-descriptor ObjectDescriptor OPTIONAL,
  encoding              CHOICE {
    single-ASN1-type      [0] EXPLICIT ANY,
    octet-aligned         [1] IMPLICIT OCTET STRING,
    arbitrary             [2] IMPLICIT BIT STRING } }
  -- This is the pre-1994 abstract syntax of EXTERNAL, but the encoding
  -- is still based on this rather than the new abstract syntax.

-- pp358-361: TYPE-IDENTIFIER and INSTANCE OF

TYPE-IDENTIFIER ::= CLASS {
  &id       OBJECT IDENTIFIER UNIQUE,
  &Type }
WITH SYNTAX {&Type IDENTIFIED BY &id}

INSTANCE OF ObjectClass.&Type({ObjectSet}) ::=
  [UNIVERSAL 8] IMPLICIT 
  SEQUENCE { type-id ObjectClass.&id({ObjectSet}),
             value [0] EXPLICIT ObjectClass.&Type({ObjectSet})(@.type-id) }
  -- Note: encoding coincides with EXTERNAL

-- pp361-364: ABSTRACT-SYNTAX

ABSTRACT-SYNTAX ::= CLASS {
  &id       OBJECT IDENTIFIER,
  &Type,
  &property BIT STRING { handles-invalid-encodings(0) }
            DEFAULT {} }
WITH SYNTAX { &Type IDENTIFIED BY &id
              [HAS PROPERTY &property] }
}<<|


@;{

An INSTANCE OF type can be represented by the following pattern (from
@cite["Dubuisson2001"], p359):

@interaction[#:eval the-eval
(code:comment "parameterized by type representing the type identifier")
(define (InstanceOf oid->type)
  (TAG #:universal #:implicit 8
       (SEQUENCE [type-id OBJECT-IDENTIFIER]
                 [value #:explicit 0 #:dependent (oid->type type-id)])))
]

where the @racket[oid->type] argument represents the information
object set.
}


@(close-eval the-eval)
