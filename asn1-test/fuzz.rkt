#lang racket/base
(require racket/list
         racket/match
         rackunit
         asn1)
(provide (all-defined-out))

(define t
  (SEQUENCE-OF
   (CHOICE [a INTEGER] [b OCTET-STRING] [c UTF8String] [d (SET-OF INTEGER)])))

(define v1 '((a 5) (b #"abc") (c "hello!") (d (2 3 5 7))))
(define der1 (asn1->bytes/DER t v1))

;; A Fuzzing is one of
;; - (list 'flip byte-index bit-index)
;; - (list 'insert byte-index byte)
;; - (list 'delete byte-index)
;; - (list 'set byte-index byte)
;; - (list 'transpose byte-index byte-index)

(define (random-fuzzing len)
  (case (random 5)
    [(0) (list 'flip (random len) (random 8))]
    [(1) (list 'insert (random len) (random-byte))]
    [(2) (list 'delete (random len))]
    [(3) (list 'set (random len) (random-byte))]
    [(4) (list 'transpose (random len) (random len))]))

(define (random-byte) (random 256))

(define (apply-fuzzing f buf0)
  (define buf (bytes-copy buf0))
  (define len (bytes-length buf))
  (match f
    [(list 'flip i biti)
     (bytes-set! buf i (bitwise-xor (bytes-ref buf i) (arithmetic-shift 1 biti)))]
    [(list 'insert i byte)
     (set! buf (bytes-append (subbytes buf 0 i) (bytes byte) (subbytes buf i len)))]
    [(list 'delete i)
     (set! buf (bytes-append (subbytes buf 0 i) (subbytes buf (min (add1 i) len))))]
    [(list 'set i byte)
     (bytes-set! buf i byte)]
    [(list 'transpose i j)
     (define bufj (bytes-ref buf j))
     (bytes-set! buf j (bytes-ref buf i))
     (bytes-set! buf i bufj)])
  buf)

;; ----

(define (do-one-fuzz f)
  (define fuzzed (apply-fuzzing f der1))
  (bytes->asn1 t fuzzed))

(module+ main
  (for ([i 20])
    (define len (bytes-length der1))
    (define f (random-fuzzing len))
    (printf "fuzzing: ~e\n" f)
    (with-handlers ([exn? (lambda (e) (printf "=> exn: ~e\n" e))])
      (define v (do-one-fuzz f))
      (printf "=> ok: ~e\n" v))
    (newline)))

;; fixed (delete 15) => bitwise-bit-set? error
;; fixed (flip 27 1) => "<= expected real given eof"
;; fixed (delete 31)
