;; Copyright 2014 Ryan Culpepper
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
(require racket/contract
         "private/base256.rkt")
(provide (contract-out
          ;; private/base256.rkt
          [signed->base256
           (-> exact-integer? bytes?)]
          [base256->signed
           (-> bytes? exact-integer?)]
          [unsigned->base256
           (-> exact-nonnegative-integer? bytes?)]
          [base256->unsigned
           (-> bytes? exact-nonnegative-integer?)]
          ;; defined here
          [base256-normalize-signed (-> bytes? bytes?)]
          [base256-normalize-unsigned (-> bytes? bytes?)]
          [base256-unsigned->signed (-> bytes? bytes?)]
          [base256-signed-zero? (-> bytes? boolean?)]
          [base256-signed-positive? (-> bytes? boolean?)]
          [base256-signed-negative? (-> bytes? boolean?)]))

(define (base256-normalize-signed b)
  ;; Remove leading 0 bytes before positive, 255 bytes before negative
  (cond [(zero? (bytes-length b))
         #"\0"]
        [(= (bytes-ref b 0) 0)
         (let ([len-1 (sub1 (bytes-length b))])
           (let loop ([i 0])
             ;; for all k < i, (bytes-ref b k) = 0
             (if (< i len-1)
                 (cond [(= (bytes-ref b i) 0)
                        (loop (add1 i))]
                       [(< (bytes-ref b i) 128)
                        (subbytes-from b i)]
                       [else (subbytes-from b (sub1 i))])
                 (cond [(< (bytes-ref b i) 128)
                        (subbytes-from b i)]
                       [else (subbytes-from b (sub1 i))]))))]
        [(= (bytes-ref b 0) 255)
         (let ([len-1 (sub1 (bytes-length b))])
           (let loop ([i 0])
             ;; for all k < i, (bytes-ref b k) = 255
             (if (< i len-1)
                 (cond [(= (bytes-ref b i) 255)
                        (loop (add1 i))]
                       [(>= (bytes-ref b i) 128)
                        (subbytes-from b i)]
                       [else (subbytes-from b (sub1 i))])
                 (cond [(>= (bytes-ref b i) 128)
                        (subbytes-from b i)]
                       [else (subbytes-from b (sub1 i))]))))]
        [else b]))

(define (base256-normalize-unsigned b)
  (cond [(zero? (bytes-length b))
         #"\0"]
        [else
         ;; Don't trim last byte
         (let ([len-1 (sub1 (bytes-length b))])
           (let loop ([i 0])
             (if (< i len-1)
                 (if (= (bytes-ref b i) 0)
                     (loop (add1 i))
                     (subbytes-from b i))
                 (subbytes-from b i))))]))

(define (base256-unsigned->signed b)
  (if (base256-signed-negative? b)
      (bytes-append #"\0" b)
      (base256-normalize-unsigned b)))

(define (subbytes-from b start)
  (if (zero? start) b (subbytes b start)))

(define (base256-signed-zero? b)
  (for/and ([x (in-bytes b)]) (zero? x)))

(define (base256-signed-positive? b)
  (let loop ([i 0])
    (and (< i (bytes-length b))
         (cond [(= (bytes-ref b i) 0)
                (loop (add1 i))]
               [(< (bytes-ref b i) 128)
                #t]
               [else #f]))))

(define (base256-signed-negative? b)
  (and (positive? (bytes-length b))
       (>= (bytes-ref b 0) 128)))
