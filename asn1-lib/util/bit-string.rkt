;; Copyright 2020 Ryan Culpepper
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
(require racket/match
         racket/contract
         "../main.rkt")
(provide (contract-out
          [bit-string-length (-> bit-string? any)]
          [bit-string-ref (-> bit-string? exact-nonnegative-integer? any)]))

;; bit-string-length : BitString -> Nat
(define (bit-string-length bs)
  (match-define (bit-string b unused) bs)
  (- (* (bytes-length b) 8) unused))

;; bit-string-ref : BitString Nat -> Boolean
(define (bit-string-ref bs index)
  (match-define (bit-string b unused) bs)
  (define blen (bytes-length b))
  (cond [(< index (- (* blen 8) unused))
         (define bytei (quotient index 8))
         (define byte (bytes-ref b bytei))
         (define biti (remainder index 8))
         ;; In ASN.1, bit 0 is the high bit.
         (bitwise-bit-set? byte (- 7 biti))]
        [else #f]))
