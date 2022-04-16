#lang scribble/manual
@(require scribble/basic
          (for-label racket/base
                     racket/contract
                     asn1))

@title[#:version "1.2"]{ASN.1}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[asn1]

This library provides a notation for defining ASN.1 data types and
support for encoding and decoding ASN.1 values using the Basic and
Distinguished Encoding Rules (BER and DER).

@(local-table-of-contents)

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@url{https://github.com/rmculpepper/asn1}}

@bold{Copying} Licensed under the
@hyperlink["http://www.apache.org/licenses/LICENSE-2.0"]{Apache
License, Version 2.0}.

@include-section["intro.scrbl"]
@include-section["types.scrbl"]
@include-section["codec.scrbl"]
@include-section["misc.scrbl"]
@include-section["translating.scrbl"]

@bibliography[

@bib-entry[#:key "Dubuisson2001"
           #:title "ASN.1: Communication Between Heterogeneous Systems"
           #:is-book? #t
           #:author "Olivier Dubuisson"
           #:date "2001"
           #:url "https://www.oss.com/asn1/resources/books-whitepapers-pubs/asn1-books.html#dubuisson"
           @;{#:url "http://www.oss.com/asn1/resources/books-whitepapers-pubs/dubuisson-asn1-book.PDF"}]

@bib-entry[#:key "Kaliski1993"
           #:title "A Layman's Guide to a Subset of ASN.1, BER, and DER"
           #:author "Burton S. Kaliski Jr."
           #:location "RSA Laboratories Technical Note"
           #:date "November 1, 1993"
           #:url "http://luca.ntop.org/Teaching/Appunti/asn1.html"]

@bib-entry[#:key "PKCS1"
           #:title "PKCS #1: RSA Cryptography, version 2.1"
           #:author "J. Jonsson and B. Kaliski"
           #:url "http://www.ietf.org/rfc/rfc3447.txt"]

]
