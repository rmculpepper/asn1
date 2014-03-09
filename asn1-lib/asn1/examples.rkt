#lang racket/base
(require "main.rkt")
(provide (all-defined-out))

(define DsaSig (Sequence [r INTEGER] [s INTEGER]))

(define pkcs1 '((iso 1) (member-body 2) (us 840) (rsadsi 113549) (pkcs 1) 1))

(define rsaEncryption (append pkcs1 '(1)))

(define RSAPublicKey (Sequence [modulus INTEGER] [publicExponent INTEGER]))

;; ---
#|
(define ECDomainParameters
  (Choice [ecParameters ECParameters]
          [namedCurve OBJECT-IDENTIFIER]
          [implicitlyCA NULL]))

(define ECParameters
  (Sequence [version   ECPVer]
            [field     FieldID]
            [curve     Curve]
            [base      ECPoint]
            [order     INTEGER]
            [cofactor  INTEGER #:optional]))

(define FieldID
  (Sequence [fieldType OBJECT-IDENTIFIER]
            [parameters ANY]))

(define ECPVer INTEGER)

(define Curve
  (Sequence [a FieldElement]
            [b FieldElement]
            [seed BIT-STRING #:optional]))

(define FieldElement OCTET-STRING)

(define ECPoint OCTET-STRING)
|#

#|
(DER-decode-hooks
 (list (list ANY 'pre (lambda (b) (list 'ANY b)))))
|#

;; ----

;; (define T61String (Tag #:universal #:implicit 20 OCTET-STRING))

(define T61String
  (Tag #:universal #:implicit 20
       (Wrap OCTET-STRING
             #:encode string->bytes/latin-1
             #:decode bytes->string/latin-1)))
