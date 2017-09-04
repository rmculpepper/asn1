#lang info

;; ========================================
;; pkg info

(define collection "asn1")
(define deps
  '("base"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "asn1-lib"))

;; ========================================
;; collect info

(define name "asn1")
(define scribblings
  '(("scribblings/asn1.scrbl" (multi-page))))
