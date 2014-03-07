#lang scribble/doc
@(require scribble/manual
          scribble/basic
          (for-label racket/base
                     racket/contract
                     asn1))

@title{ASN.1}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[asn1]

This library provides a notation for defining ASN.1 data types and
encoding and decoding using the ``distinguished encoding rules''
(DER).

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
