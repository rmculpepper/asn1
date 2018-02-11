#lang info

;; ========================================
;; pkg info

(define version "1.0")
(define collection "asn1")
(define deps '("base"))
(define build-deps
  '("racket-doc" "scribble-lib" "asn1-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "asn1")
(define scribblings
  '(("scribblings/asn1.scrbl" (multi-page))))
