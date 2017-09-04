#lang scribble/manual
@(require scribble/basic
          (for-label racket/base
                     racket/contract
                     asn1))

@title{ASN.1}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[asn1]

This library provides a notation for defining ASN.1 data types and
encoding and decoding using the ``distinguished encoding rules''
(DER).

@(local-table-of-contents)

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@url{https://github.com/rmculpepper/asn1}}

@bold{Copying} This program is free software: you can redistribute
it and/or modify it under the terms of the
@hyperlink["http://www.gnu.org/licenses/lgpl.html"]{GNU Lesser General
Public License} as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License and GNU Lesser General Public License for more
details.

@include-section["intro.scrbl"]
@include-section["types.scrbl"]
@include-section["der.scrbl"]
@include-section["translating.scrbl"]
@include-section["util.scrbl"]

@bibliography[

@bib-entry[#:key "Dubuisson2001"
           #:title "ASN.1: Communication Between Heterogeneous Systems"
           #:is-book? #t
           #:author "Olivier Dubuisson"
           #:date "2001"]

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
