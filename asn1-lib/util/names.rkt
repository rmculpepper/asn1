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
(require "../main.rkt"
         (only-in "../private/ber.rkt" encode-bad decode-bad))
(provide WRAP-NAMES)

(define (WRAP-NAMES t named-values #:who [who 'WRAP-NAMES])
  (cond [(equal? t ENUMERATED)
         (ENUMERATED/names named-values #:who who)]
        [(equal? t BIT-STRING)
         (BIT-STRING/names named-values #:who who)]
        [else (error 'WRAP-NAMES "unsupported types\n  type: ~e" t)]))

;; NamedValues = (Listof (cons Symbol Any))

(define (xassoc v alist)
  (for/first ([e (in-list alist)] #:when (equal? v (cdr e))) e))

(define (ENUMERATED/names named-values #:who [who 'ENUMERATED/names])
  (WRAP ENUMERATED
        #:encode (lambda (v)
                   (cond [(symbol? v)
                          (cdr (or (assq v named-values) (error who "unknown name: ~e" v)))]
                         [(exact-nonnegative-integer? v) v]
                         [else (encode-bad who v)]))
        #:decode (lambda (v) (cond [(xassoc v named-values) => car] [else v]))))

(define (BIT-STRING/names named-bits #:who [who 'BIT-STRING/names])
  (define (bit-index->name x) (cond [(xassoc x named-bits) => car] [else x]))
  (define (byte-bit-ref byte biti) (bitwise-bit-set? byte (- 7 biti)))
  (define (byte-bit-set byte biti) (bitwise-ior byte (arithmetic-shift 1 (- 7 biti))))
  (WRAP BIT-STRING
        #:encode (lambda (v)
                   (define (get-bit-index x)
                     (cond [(symbol? x)
                            (cdr (or (assq x named-bits) (error who "unknown name: ~e" x)))]
                           [(exact-nonnegative-integer? x) x]
                           [else (encode-bad who v)]))
                   (cond [(null? v) (bit-string #"" 0)]
                         [(list? v) ;; non-empty
                          (define bit-indexes (map get-bit-index v))
                          (define max-bit (apply max bit-indexes))
                          (define bits (make-bytes (add1 (quotient max-bit 8)) 0))
                          (for ([bit-index (in-list bit-indexes)])
                            (define bytei (quotient bit-index 8))
                            (define biti (remainder bit-index 8))
                            (bytes-set! bits bytei (byte-bit-set (bytes-ref bits bytei) biti)))
                          (bit-string bits (- 7 (quotient max-bit 8)))]
                         [(bit-string? v) v]
                         [else (encode-bad who v)]))
        #:decode (lambda (v)
                   (define bs (bit-string-bytes v))
                   (define bslen (bytes-length bs))
                   (define unused (bit-string-unused v))
                   (for/list ([byte (in-bytes bs)]
                              [bytei (in-naturals)]
                              #:when #t
                              [biti (in-range (- 8 (if (= bytei (sub1 bslen)) unused 0)))]
                              #:when (byte-bit-ref byte biti))
                     (bit-index->name (+ (* bytei 8) biti))))))