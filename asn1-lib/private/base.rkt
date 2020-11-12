;; Copyright 2014-2017 Ryan Culpepper
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

;; Base types and tags.

#lang racket/base
(provide (all-defined-out))

;; ============================================================
;; Error Reporting

(define asn1-who-key (gensym 'asn1-who))

(define (asn1-who) (or (continuation-mark-set-first #f asn1-who-key) 'asn1:???))

(define (with-who who proc)
  (if (continuation-mark-set-first #f asn1-who-key)
      (proc)
      (with-continuation-mark asn1-who-key who (proc))))

(define (asn1-error fmt . args) (apply error (asn1-who) fmt args))

(define (BER-error message [fmt ""] . args)
  (error (asn1-who) "violation of the Basic Encoding Rules (BER);\n ~a~a"
         message (apply format fmt args)))

(define (DER-error message [fmt ""] . args)
  (error (asn1-who) "violation of the Distinguished Encoding Rules (DER);\n ~a~a"
         message (apply format fmt args)))


;; ============================================================
;; Tags

;; A Tag is a Nat encoding a TagClass and a Nat (the tag number)
;; - the lowest two bits encode the class:
;;   (0,0)='universal, (0,1)='application, (1,0)='context-sensitive, (1,1)='private

;; make-tag : TagClass Nat -> Tag
(define (make-tag tc tn) (+ (tagclass->bits tc) (arithmetic-shift tn 2)))

;; tag-class : Tag -> TagClass
(define (tag-class t) (bits->tagclass (bitwise-bit-field t 0 2)))

;; tag-index : Tag -> Nat
(define (tag-index t) (arithmetic-shift t -2))

;; tag->class+index : Tag -> (values TagClass Nat)
(define (tag->class+index t) (values (tag-class t) (tag-index t)))

;; tagclass<->bits : TagClass <-> Nat
(define (tagclass->bits tc)
  (case tc
    [(universal)        #b00] [(application) #b01]
    [(context-specific) #b10] [(private)     #b11]))
(define (bits->tagclass n)
  (case n
    [(#b00)        'universal] [(#b01) 'application]
    [(#b10) 'context-specific] [(#b11)     'private]))

;; display-tag : Tag -> String
(define (display-tag t)
  (format "~a ~a~a" (tag-class t) (tag-index t)
          (cond [(eq? (tag-class t) 'universal)
                 (case (tag-index t)
                   [(1) " (BOOLEAN)"]
                   [(2) " (INTEGER)"]
                   [(3) " (BIT STRING)"]
                   [(4) " (OCTET STRING)"]
                   [(5) " (NULL)"]
                   [(6) " (OBJECT IDENTIFIER)"]
                   [(9) " (REAL)"]
                   [(10) " (ENUMERATED)"]
                   [(13) " (RELATIVE OID)"]
                   [(16) " (SEQUENCE)"]
                   [(17) " (SET)"]
                   [(18) " (NumericString)"]
                   [(19) " (PrintableString)"]
                   [(22) " (IA5String)"]
                   [(23) " (UTCTime)"]
                   [(26) " (VisibleString)"]
                   [(28) " (UniversalString)"]
                   [(30) " (BMPString)"]
                   [(12) " (UTF8String)"]
                   [(24) " (GeneralizedTime)"]
                   [else ""])]
                [else ""])))

;; nil-tag : Tag
(define nil-tag (make-tag 'universal 0))

;; tag{<,<=}? : Tag Tag -> Boolean
(define (tag<? a b)
  (let ([ac (bitwise-bit-field a 0 2)] [bc (bitwise-bit-field b 0 2)])
    (or (< ac bc) (and (= ac bc) (< (tag-index a) (tag-index b))))))
(define (tag<=? a b) (or (= a b) (tag<? a b)))

;; ============================================================
;; Base Types and Tags

;; Reference: http://luca.ntop.org/Teaching/Appunti/asn1.html

;; A BaseType is one of the Symbols in the table below.

(define (base-type-tag base-type)
  (make-tag 'universal (base-type-tagn base-type)))

(define (base-type-tagn base-type)
  (case base-type
    [(BOOLEAN)             1]
    [(INTEGER)             2]
    [(BIT-STRING)          3]
    [(OCTET-STRING)        4]
    [(NULL)                5]
    [(OBJECT-IDENTIFIER)   6]
    [(REAL)                9] ;; Not supported
    [(ENUMERATED)         10]
    [(RELATIVE-OID)       13]
    [(SEQUENCE)           16]
    [(SET)                17]
    [(NumericString)      18]
    [(PrintableString)    19]
    [(IA5String)          22]
    [(UTCTime)            23] ;; Not supported
    [(VisibleString)      26]
    [(UniversalString)    28] ;; UCS-4
    [(BMPString)          30] ;; UCS-2
    [(UTF8String)         12] ;; UTF-8
    [(GeneralizedTime)    24] ;; Not supported
    [else (error 'base-type-tagn "unknown base type: ~e" base-type)]))

;; base-type-cons-ok? : BaseType Boolean Boolean -> Boolean
(define (base-type-cons-ok? base-type der? cons?)
  (case (base-type-cons-mode base-type)
    [(P) (eq? cons? #f)]
    [(C) (eq? cons? #t)]
    [(PC) (if der? (eq? cons? #f) #t)]))

;; base-type-cons-mode : BaseType -> (U 'P 'C 'PC)
(define (base-type-cons-mode base-type)
  (case base-type
    [(BOOLEAN)            'P]
    [(INTEGER)            'P]
    [(BIT-STRING)         'PC]
    [(OCTET-STRING)       'PC]
    [(NULL)               'P]
    [(OBJECT-IDENTIFIER)  'P]
    [(REAL)               'P]  ;; Not supported
    [(ENUMERATED)         'P]
    [(RELATIVE-OID)       'P]
    [(SEQUENCE)           'C]
    [(SET)                'C]
    [(NumericString)      'PC]
    [(PrintableString)    'PC]
    [(IA5String)          'PC]
    [(UTCTime)            'P]  ;; Not supported
    [(VisibleString)      'PC]
    [(UniversalString)    'PC] ;; UCS-4
    [(BMPString)          'PC] ;; UCS-2
    [(UTF8String)         'PC] ;; UTF-8
    [(GeneralizedTime)    'P]  ;; Not supported
    [else (error 'base-type-cons-mode "unknown base type: ~e" base-type)]))

#|
;; An Entry is (list BaseType Nat PC)
;; (define base-type-db
;;   '(;; Type           TagN   Constructed?
;;     [BOOLEAN             1   #f]
;;     [INTEGER             2   #f]
;;     [BIT-STRING          3   #f]
;;     [OCTET-STRING        4   #f]
;;     [NULL                5   #f]
;;     [OBJECT-IDENTIFIER   6   #f]
;;     ;; [REAL             9   #f] ;; Weird, prob. not worth the effort.
;;     [ENUMERATED         10   #f]
;;     [RELATIVE-OID       13   #f]
;;     [SEQUENCE           16   #t]
;;     [SET                17   #t]
;;     [PrintableString    19   #f]
;;     [IA5String          22   #f]
;;     ;; [UTCTime         23   #f]
;;     ;; [UniversalString 28   #f] ;; UCS4
;;     ;; [BMPString       30   #f] ;; UCS2
;;     [UTF8String         12   #f] ;; UTF8
;;     ;; [GeneralizedTime 24   #f]
;;     ))

;; base-type->entry : BaseType -> Entry
(define (base-type->entry type)
  (for/or ([entry (in-list base-type-db)])
    (and (eq? type (entry-type entry)) entry)))

;; base-type->tagn : BaseType -> Nat
(define (base-type->tagn type)
  (entry-tagn (or (base-type->entry type)
                  (error 'base-type->tagn "unknown base type: ~e" type))))

;; tag->entry : Tag[Universal] -> Entry
(define (tag->entry t)
  (and (eq? (tag-class t) 'universal)
       (let ([tagn (tag-index t)])
         (for/or ([entry (in-list base-type-db)])
           (and (equal? tagn (tag-entry-tagn entry)) entry)))))

(define (entry-type te) (car te))
(define (entry-tagn te) (cadr te))
(define (entry-tag te)  (make-tag 'universal (tag-entry-tagn te)))
(define (entry-cons? te) (caddr te))
|#

;; ============================================================
;; Base Type Support

;; ascii-string? : Any -> Boolean
(define (ascii-string? s)
  (and (string? s) (regexp-match? #px"^[[:ascii:]]*$" s)))

;; asn1-printable-string? : Any -> Boolean
(define (asn1-printable-string? s)
  (and (string? s) (regexp-match? #rx"^[-a-zA-Z0-9 '()+,./:=?]*$" s)))

(define (asn1-numeric-string? s)
  (and (string? s) (regexp-match? #rx"^[ 0-9]*$" s)))

(define (asn1-visible-string? s)
  (and (string? s) (regexp-match? #rx"^[\x20-\x7E]*$" s)))

;; ----------------------------------------
;; INTEGER:
;; base-256, two's-complement (!!), most significant octet first
;; zero encoded as 1 octet

;; NULL:
;; NULL has empty value encoding; ignore arg when encoding, return #f on decode

;; OBJECT IDENTIFIER:
;; If OID = c1, c2, ... cN, then
;; first octet is 40*c1 + c2
;; following octets are c3, ... cN encoded as follows:
;;   base-128, most-significant first, high bit set on all but last octet of encoding

;; ----------------------------------------
;; BIT STRING

;; A Bit-String is (bit-string Bytes Nat)
;; bytes is bytestring; bit 0 of bitstring is high bit (value 128) of first octet
;; unused in [0,7], indicates how many low bits in last octet are meaningless
;; bit-length is (bytes-length bytes) * 8 - unused
(struct bit-string (bytes unused) #:transparent)
