;; Copyright 2014-2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/promise
         racket/match
         binaryio/bytes
         binaryio/unchecked/reader
         "private/base.rkt"
         "private/types.rkt"
         "private/ber.rkt"
         "private/ber-frame.rkt")

(provide SEQUENCE
         SET
         CHOICE
         TAG
         DELAY
         (contract-out
          [SEQUENCE-OF
           (-> asn1-type? asn1-type?)]
          [SET-OF
           (-> asn1-type? asn1-type?)]
          [WRAP
           (->* [asn1-type?]
                [#:encode (-> any/c any/c)
                 #:decode (-> any/c any/c)]
                asn1-type?)])

         define-asn1-type

         (contract-out
          [read-asn1  (->* [asn1-type?] [input-port? #:rules rules/c] any)]
          [write-asn1 (->* [asn1-type? any/c] [output-port? #:rules rules/c] void?)]
          [bytes->asn1 (->* [asn1-type? bytes?] [#:rules rules/c] any)]
          [asn1->bytes (->* [asn1-type? any/c] [#:rules rules/c] bytes?)]

          [read-asn1/DER  (->* [asn1-type?] [input-port?] any)]
          [write-asn1/DER (->* [asn1-type? any/c] [output-port?] void?)]
          [bytes->asn1/DER (-> asn1-type? bytes? any)]
          [asn1->bytes/DER (-> asn1-type? any/c bytes?)])

         ;; private/types.rkt
         asn1-type?
         ANY
         BOOLEAN
         INTEGER
         ENUMERATED
         BIT-STRING
         OCTET-STRING
         NULL
         OBJECT-IDENTIFIER
         RELATIVE-OID
         PrintableString
         IA5String
         UTF8String
         NumericString
         VisibleString
         UniversalString
         BMPString

         GeneralizedTime
         UTCTime

         asn1-utc-time?
         asn1-generalized-time?

         OID
         build-OID

         ANY*

         ;; private/base.rkt
         (struct-out exn:fail:asn1)
         (struct-out exn:fail:asn1:encoding)
         (struct-out exn:fail:asn1:type)
         asn1-printable-string?
         asn1-numeric-string?
         asn1-visible-string?
         asn1-oid?

         ascii-string?
         (contract-out
          [struct bit-string ([bytes bytes?] [unused (integer-in 0 7)])]))

(define rules/c (or/c 'BER 'DER))

;; ============================================================

(begin-for-syntax

  (define-splicing-syntax-class tag-class
    (pattern (~seq #:universal) #:with tagclass #'universal)
    (pattern (~seq #:private)   #:with tagclass #'private)
    (pattern (~seq #:application) #:with tagclass #'application)
    (pattern (~seq) #:with tagclass #'context-specific))

  (define-splicing-syntax-class tag
    (pattern (~seq :tag-class tagn:nat)
             #:with e #'(make-tag 'tagclass 'tagn)))

  (define-splicing-syntax-class tag-clause
    #:attributes (mode e)
    (pattern (~seq #:explicit :tag)
             #:with mode 'explicit)
    (pattern (~seq #:implicit :tag)
             #:with mode 'implicit)
    (pattern (~seq)
             #:with mode #f
             #:with e #'#f))

  (define-splicing-syntax-class option-clause
    (pattern (~seq #:optional)
             #:with option #''(optional))
    (pattern (~seq #:default v:expr)
             #:with option #'(list 'default v))
    (pattern (~seq)
             #:with option #''#f))

  (define-splicing-syntax-class extensible
    #:attributes (ext)
    (pattern (~seq #:extensible ext:id))
    (pattern (~seq) #:with ext #'#f))

  (define (in-rprefixes lst)
    ;; Produces list same length as lst
    (let loop ([lst lst] [rprefix null])
      (cond [(pair? lst)
             (cons rprefix (loop (cdr lst) (cons (car lst) rprefix)))]
            [(null? lst)
             null]))))

(define-syntax (SEQUENCE stx)
  (define-syntax-class sequence-component
    #:attributes (name type0 option dep)
    (pattern [name:id t:tag-clause type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with type0 #'(type-add-tag 'SEQUENCE type.c 't.mode t.e)
             #:with dep #'#f)
    (pattern [name:id t:tag-clause #:dependent type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with type0 #'(type-add-tag 'SEQUENCE ANY 't.mode t.e #t)
             #:with dep #'(type-add-tag 'SEQUENCE type.c 't.mode t.e)))
  (define (wrap-refine names dep)
    (syntax-parse dep
      [#f #f]
      [dep (with-syntax ([(name ...) names])
             #'(lambda (h) (let ([name (hash-ref h 'name #f)] ...) dep)))]))
  (syntax-parse stx
    [(SEQUENCE c:sequence-component ... e:extensible)
     #`(asn1-type:sequence
        (check-sequence-components
         'SEQUENCE
         (map component-add-refine
              (list (make-component 'c.name c.type0 c.option) ...)
              (list #,@(for/list ([prefix-names (in-rprefixes (syntax->list #'(c.name ...)))]
                                  [c-dep (syntax->list #'(c.dep ...))])
                         (wrap-refine prefix-names c-dep)))))
        'e.ext)]))

(define-syntax (SET stx)
  (define-syntax-class set-component
    #:attributes (name e)
    (pattern [name:id t:tag-clause type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-component 'name (type-add-tag 'Set type.c 't.mode t.e) option)))
  (syntax-parse stx
    [(SET c:set-component ... e:extensible)
     #'(asn1-type:set (check-set-components 'SET (list c.e ...)) 'e.ext)]))

(define-syntax (CHOICE stx)
  (define-syntax-class variant
    (pattern [name:id t:tag-clause type]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-variant 'name (type-add-tag 'CHOICE type.c 't.mode t.e))))
  (syntax-parse stx
    [(CHOICE v:variant ... e:extensible)
     #'(asn1-type:choice (check-choice-variants 'CHOICE (list v.e ...)) 'e.ext)]))

(define-syntax TAG
  (syntax-parser
    [(TAG #:explicit t:tag type)
     #:declare type (expr/c #'asn1-type?)
     #'(type-add-tag 'TAG type.c 'explicit t.e)]
    [(TAG #:implicit t:tag type)
     #:declare type (expr/c #'asn1-type?)
     #'(type-add-tag 'TAG type.c 'implicit t.e)]))

(define-syntax DELAY
  (syntax-parser
    [(DELAY type)
     #:declare type (expr/c #'asn1-type?)
     #'(asn1-type:delay (delay type))]))

(define (SEQUENCE-OF type)
  (asn1-type:sequence-of type))

(define (SET-OF type)
  (asn1-type:set-of type))

(define (WRAP type
              #:encode [encode-f #f]
              #:decode [decode-f #f])
  (asn1-type:wrap type encode-f decode-f))

;; ============================================================

(define ANY (asn1-type:any))
(define BOOLEAN (make-base-type 'BOOLEAN))
(define INTEGER (make-base-type 'INTEGER))
(define BIT-STRING (make-base-type 'BIT-STRING))
(define OCTET-STRING (make-base-type 'OCTET-STRING))
(define NULL (make-base-type 'NULL))
(define OBJECT-IDENTIFIER (make-base-type 'OBJECT-IDENTIFIER))
(define RELATIVE-OID (make-base-type 'RELATIVE-OID))
(define ENUMERATED (make-base-type 'ENUMERATED))
(define NumericString (make-base-type 'NumericString))
(define PrintableString (make-base-type 'PrintableString))
(define IA5String (make-base-type 'IA5String))
(define VisibleString (make-base-type 'VisibleString))
(define UniversalString (make-base-type 'UniversalString))
(define BMPString (make-base-type 'BMPString))
(define UTF8String (make-base-type 'UTF8String))

;; ============================================================

;; Convert to string, check regexp
(define GeneralizedTime
  (TAG #:implicit #:universal 24
       (WRAP IA5String
             #:encode (lambda (s)
                        (cond [(regexp-match? GeneralizedTime-rx s) s]
                              [else (encode-bad 'GeneralizedTime s)]))
             #:decode (lambda (s)
                        (cond [(regexp-match? GeneralizedTime-rx s) s]
                              [else (decode-bad 'GeneralizedTime s)])))))

(define UTCTime
  (TAG #:implicit #:universal 23
       (WRAP IA5String
             #:encode (lambda (s)
                        (cond [(regexp-match? UTCTime-rx s) s]
                              [else (encode-bad 'UTCTime s)]))
             #:decode (lambda (s)
                        (cond [(regexp-match? UTCTime-rx s) s]
                              [else (decode-bad 'UTCTime s)])))))

(module rx racket/base
  (require racket/string)
  (provide (all-defined-out))
  (define (rx . parts) (format "(?:~a)" (apply string-append parts)))
  (define (rx-select . parts) (format "(~a)" (apply string-append parts)))
  (define (rx? . parts) (format "(?:~a)?" (apply string-append parts)))
  (define (rx-or . alts) (rx (string-join (map rx alts) "|"))))
(require (submod "." rx))

;; GeneralizedTime = YYYYMMDDhh[mm[ss[<sep>f[f[f[f]]]]]]<offset>
;; offset = ε | Z | +hhmm | -hhmm
(define MMDD-rx
  (rx-select
   (rx-or (rx (rx "01|03|05|07|08|10|12") (rx "0[1-9]|[12][0-9]|3[0-1]"))
          (rx (rx "04|06|09|11") (rx "0[1-9]|[12][0-9]|30"))
          (rx (rx "02") (rx "0[1-9]|[12][0-9]")))))

(define hhmmssf-rx  ;; allow fractional seconds
  (rx (rx-select "[01][0-9]|2[0-3]") ;; hh
      (rx? (rx-select "[0-5][0-9]") ;; mm
           (rx? (rx-select "[0-5][0-9]|60") ;; ss -- allow leap second
                (rx? "[.,]" (rx-select "[0-9]+"))))))

(define hhmmss-rx  ;; fractional seconds
  (rx (rx-select "[01][0-9]|2[0-3]") ;; hh
      (rx? (rx-select "[0-5][0-9]") ;; mm
           (rx? (rx-select "[0-5][0-9]|60"))))) ;; ss -- allow leap second

(define offset-rx
  (rx-or "Z" (rx "[-+]" (rx-select (rx "[01][0-9]|2[0-3]") (rx? "[0-5][0-9]")))))

(define GeneralizedTime-rx
  (pregexp (string-append "^([0-9]{4})" MMDD-rx hhmmssf-rx (rx? offset-rx) "$")))

;; UTCTime = like GeneralizedTime, but 2 chars for YY, no frac, offset required
(define UTCTime-rx
  (pregexp (string-append "^([0-9]{2})" MMDD-rx hhmmss-rx offset-rx "$")))

(module+ private-util-time
  (provide GeneralizedTime-rx
           UTCTime-rx))

(define (asn1-utc-time? v)
  (and (string? v) (regexp-match? UTCTime-rx v)))
(define (asn1-generalized-time? v)
  (and (string? v) (regexp-match? GeneralizedTime-rx v)))

;; ============================================================

(define ANY*
  (let ([REC (DELAY ANY*)])
    (CHOICE (boolean    BOOLEAN)
            (integer    INTEGER)
            (bits       BIT-STRING)
            (octets     OCTET-STRING)
            (null       NULL)
            (oid        OBJECT-IDENTIFIER)
            (rel-oid    RELATIVE-OID)
            (enum       ENUMERATED)
            (ia5string  IA5String)
            (utf8       UTF8String)
            (sequence   (SEQUENCE-OF REC))
            (set        (SET-OF REC))
            (numeric-string    NumericString)
            (printable-string  PrintableString)
            (visible-string    VisibleString)
            (universal-string  UniversalString)
            (bmp-string        BMPString)
            (generalized-time  GeneralizedTime)
            (utc-time          UTCTime)
            #:extensible any)))

;; ============================================================

(begin-for-syntax
 (define-syntax-class OID-component
   (pattern n:nat)
   (pattern (s:identifier n:nat))))

(define-syntax OID
  (syntax-parser
   [(OID c:OID-component ...)
    #'(quote (c.n ...))]))

(define-syntax build-OID
  (syntax-parser
   [(OID base c:OID-component ...)
    #:declare base (expr/c #'(listof exact-nonnegative-integer?))
    #'(append base.c (quote (c.n ...)))]))

;; ============================================================

(define-syntax define-asn1-type
  (syntax-parser
   [(define-asn1-type name:id type)
    #:declare type (expr/c #'asn1-type?)
    #'(define name (DELAY type.c))]))

;; ============================================================

(define (read-asn1 type [in (current-input-port)]
                   #:check-exhausted? [check-exhausted? #f]
                   #:rules [rules 'BER]
                   #:limit [limit #f]
                   #:who [who 'read-asn1])
  (with-who who
    (lambda ()
      (define der? (eq? rules 'DER))
      (define br (make-asn1-binary-reader in #:limit limit))
      (begin0 (decode-frame type (read-frame br der?) der?)
        (when check-exhausted? (b-check-exhausted br "ASN.1 value" #:who who))))))

(define (write-asn1 type value [out (current-output-port)]
                    #:rules [rules 'BER] #:who [who 'write-asn1])
  (with-who who
    (lambda ()
      (let ([der? (eq? rules 'DER)])
        (write-frame (BER-encode type value #:der? der?) out der?)))))

(define (bytes->asn1 type b
                     #:rules [rules 'BER]
                     #:who [who 'bytes->asn1])
  (read-asn1 type (open-input-bytes b) #:rules rules #:who who
             #:check-exhausted? #t
             #:limit (bytes-length b)))

(define (asn1->bytes type v #:rules [rules 'BER] #:who [who 'asn1->bytes])
  (with-who who
    (lambda ()
      (define out (open-output-bytes))
      (write-asn1 type v out #:rules rules)
      (get-output-bytes out))))

(define (read-asn1/DER type [in (current-input-port)])
  (read-asn1 type in #:rules 'DER #:who 'read-asn1/DER))
(define (write-asn1/DER type value [out (current-output-port)])
  (write-asn1 type value out #:rules 'DER #:who 'write-asn1/DER))
(define (bytes->asn1/DER type b)
  (bytes->asn1 type b #:rules 'DER #:who 'bytes->asn1/DER))
(define (asn1->bytes/DER type v)
  (asn1->bytes type v #:rules 'DER #:who 'asn1->bytes/DER))
