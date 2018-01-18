;; Copyright 2017 Ryan Culpepper
;; 
;; This library is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library.  If not, see <http://www.gnu.org/licenses/>.

#lang racket/base
(require racket/contract/base
         "private/types.rkt"
         "private/ber-frame.rkt"
         "private/ber.rkt")
(provide (contract-out
          [struct BER-frame
                  ([tag exact-nonnegative-integer?]
                   [content (or/c bytes? (listof (or/c bytes? BER-frame?)))])]
          [read-BER-frame
           (->* [] [input-port? #:der? any/c #:limit (or/c exact-nonnegative-integer? +inf.0)]
                BER-frame?)]
          [write-BER-frame
           (->* [BER-frame?] [output-port? #:der? any/c] void?)]
          [BER-encode (->* [asn1-type? any/c] [#:der? any/c] BER-frame?)]
          [BER-decode (->* [asn1-type? BER-frame?] [#:der? any/c] any/c)]))
