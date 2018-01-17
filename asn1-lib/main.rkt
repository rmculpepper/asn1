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

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/promise
         racket/match
         binaryio/bytes
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
                [#:pre-encode (-> any/c any/c)
                 #:encode (-> any/c bytes?)
                 #:decode (-> bytes? any/c)
                 #:post-decode (-> any/c any/c)]
                asn1-type?)])

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

         OID
         build-OID

         define-asn1-type

         (contract-out
          [read-asn1  (->* [asn1-type?] [input-port? #:rules rules/c] any)]
          [write-asn1 (->* [asn1-type? any/c] [output-port? #:rules rules/c] void?)]
          [bytes->asn1 (->* [asn1-type? bytes?] [#:rules rules/c] any)]
          [asn1->bytes (->* [asn1-type? any/c] [#:rules rules/c] bytes?)])

         ;; private/base-types.rkt
         printable-string?
         ascii-string?

         (contract-out
          ;; private/base-types.rkt
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
             #:with type0 #'(type-add-tag 'SEQUENCE ANY 't.mode t.e)
             #:with dep #'(type-add-tag 'SEQUENCE type.c 't.mode t.e)))
  (define (wrap-refine names dep)
    (syntax-parse dep
      [#f #f]
      [dep (with-syntax ([(name ...) names])
             #'(lambda (h) (let ([name (hash-ref h 'name #f)] ...) dep)))]))
  (syntax-parse stx
    [(SEQUENCE c:sequence-component ...)
     #`(asn1-type:sequence
        (check-sequence-components
         'SEQUENCE
         (map component-add-refine
              (list (make-component 'c.name c.type0 c.option) ...)
              (list #,@(for/list ([prefix-names (in-rprefixes (syntax->list #'(c.name ...)))]
                                  [c-dep (syntax->list #'(c.dep ...))])
                         (wrap-refine prefix-names c-dep))))))]))

(define-syntax (SET stx)
  (define-syntax-class set-component
    #:attributes (name e)
    (pattern [name:id t:tag-clause type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-component 'name (type-add-tag 'Set type.c 't.mode t.e) option)))
  (syntax-parse stx
    [(SET c:set-component ...)
     #'(asn1-type:set (check-set-components 'SET (list c.e ...)))]))

(define-syntax (CHOICE stx)
  (define-syntax-class variant
    (pattern [name:id t:tag-clause type]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-variant 'name (type-add-tag 'CHOICE type.c 't.mode t.e))))
  (syntax-parse stx
    [(CHOICE v:variant ...)
     #'(asn1-type:choice (check-choice-variants 'CHOICE (list v.e ...)))]))

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
              #:pre-encode [pre-encode-f #f]
              #:encode [encode-f #f]
              #:decode [decode-f #f]
              #:post-decode [post-decode-f #f])
  (define (bad)
    (error 'WRAP "cannot add encode/decode hook to non-base type\n  type: ~e\n  hook: ~e"
           type (or encode-f decode-f)))
  (define type*
    (cond [(or encode-f decode-f)
           (let loop ([type type])
             (match type
               [(asn1-type:base name tag encode0 decode0)
                (asn1-type:base name tag (or encode-f encode0) (or decode-f decode0))]
               [(asn1-type:implicit-tag tag type)
                (asn1-type:implicit-tag tag (loop type))]
               [_ (bad)]))]
          [else type]))
  (asn1-type:wrap type* pre-encode-f post-decode-f))

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
(define PrintableString (make-base-type 'PrintableString))
;; T61String
(define IA5String (make-base-type 'IA5String))
;; UTCTime
(define UTF8String (make-base-type 'UTF8String))

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

(define (read-asn1 type [in (current-input-port)] #:rules [rules 'BER])
  (with-who 'read-asn1
    (lambda ()
      (define der? (eq? rules 'DER))
      (define f (read-BER-frame in #:der? der?))
      (BER-decode type f #:der? der?))))

(define (write-asn1 type value [out (current-output-port)] #:rules [rules 'BER])
  (with-who 'write-asn1
    (lambda ()
      (define der? (eq? rules 'DER))
      (define f (BER-encode type value #:der? der?))
      (write-BER-frame f out #:der? der?))))

(define (bytes->asn1 type b #:rules [rules 'BER])
  (with-who 'bytes->asn1
    (lambda ()
      (read/exhaust 'bytes->asn1 "ASN1 value"
                    (lambda (in) (read-asn1 type in #:rules rules))
                    (open-input-bytes b)))))

(define (asn1->bytes type v #:rules [rules 'BER])
  (with-who 'asn1->bytes
    (lambda ()
      (define out (open-output-bytes))
      (write-asn1 type v out #:rules rules)
      (get-output-bytes out))))
