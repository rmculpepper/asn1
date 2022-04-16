;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

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
