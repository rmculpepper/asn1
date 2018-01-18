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
         "private/base.rkt"
         "private/types.rkt"
         "private/ber.rkt"
         "private/ber-frame.rkt")
(provide Sequence
         Set
         Choice
         Tag
         Delay
         (contract-out
          [SequenceOf
           (-> asn1-type? asn1-type?)]
          [SetOf
           (-> asn1-type? asn1-type?)]
          [Wrap
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

         ;; private/base-types.rkt
         printable-string?
         ascii-string?

         (contract-out
          ;; private/base-types.rkt
          [struct bit-string ([bytes bytes?] [unused (integer-in 0 7)])]

          ;; private/ber.rkt
          [BER-encode (->* [asn1-type? any/c] [#:der? any/c] BER-frame?)]
          [BER-decode (->* [asn1-type? BER-frame?] [#:der? any/c] any/c)]

          #|
          [read-asn1  (->* [asn1-type?] [input-port? #:der? any/c] any)]
          [write-asn1 (->* [asn1-type? any/c] [output-port? #:der? any/c] void?)]

          [bytes->asn1 (->* [bytes?] [#:der? any/c] any)]
          [asn1->bytes (->* [any/c] [#:der? any/c] bytes?)]
          |#

          ;; private/ber-frame.rkt
          [struct BER-frame
                  ([tag exact-nonnegative-integer?]
                   [content (or/c bytes? (listof (or/c bytes? BER-frame?)))])]
          [read-BER-frame
           (->* [] [input-port? #:der? any/c #:limit (or/c exact-nonnegative-integer? +inf.0)]
                BER-frame?)]
          [write-BER-frame
           (->* [BER-frame?] [output-port? #:der? any/c] void?)]
          ))

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

(define-syntax (Sequence stx)
  (define-syntax-class sequence-component
    #:attributes (name type0 option dep)
    (pattern [name:id t:tag-clause type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with type0 #'(type-add-tag 'Sequence type.c 't.mode t.e)
             #:with dep #'#f)
    (pattern [name:id t:tag-clause #:dependent type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with type0 #'(type-add-tag 'Sequence ANY 't.mode t.e)
             #:with dep #'(type-add-tag 'Sequence type.c 't.mode t.e)))
  (define (wrap-refine names dep)
    (syntax-parse dep
      [#f #f]
      [dep (with-syntax ([(name ...) names])
             #'(lambda (h) (let ([name (hash-ref h 'name #f)] ...) dep)))]))
  (syntax-parse stx
    [(Sequence c:sequence-component ...)
     #`(asn1-type:sequence
        (check-sequence-components
         'Sequence
         (map component-add-refine
              (list (make-component 'c.name c.type0 c.option) ...)
              (list #,@(for/list ([prefix-names (in-rprefixes (syntax->list #'(c.name ...)))]
                                  [c-dep (syntax->list #'(c.dep ...))])
                         (wrap-refine prefix-names c-dep))))))]))

(define-syntax (Set stx)
  (define-syntax-class set-component
    #:attributes (name e)
    (pattern [name:id t:tag-clause type :option-clause]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-component 'name (type-add-tag 'Set type.c 't.mode t.e) option)))
  (syntax-parse stx
    [(Set c:set-component ...)
     #'(asn1-type:set (check-set-components (list c.e ...)))]))

(define-syntax (Choice stx)
  (define-syntax-class variant
    (pattern [name:id t:tag-clause type]
             #:declare type (expr/c #'asn1-type?)
             #:with e #'(make-variant 'name (type-add-tag 'Choice type.c 't.mode t.e))))
  (syntax-parse stx
    [(Choice v:variant ...)
     #'(asn1-type:choice (check-choice-variants (list v.e ...)))]))

(define-syntax Tag
  (syntax-parser
    [(Tag #:explicit t:tag type)
     #:declare type (expr/c #'asn1-type?)
     #'(type-add-tag 'Tag type.c 'explicit t.e)]
    [(Tag #:implicit t:tag type)
     #:declare type (expr/c #'asn1-type?)
     #'(type-add-tag 'Tag type.c 'implicit t.e)]))

(define-syntax Delay
  (syntax-parser
    [(Delay type)
     #:declare type (expr/c #'asn1-type?)
     #'(asn1-type:delay (delay type))]))

(define (SequenceOf type)
  (asn1-type:sequence-of type))

(define (SetOf type)
  (asn1-type:set-of type))

(define (Wrap type
              #:pre-encode [pre-encode-f #f]
              #:encode [encode-f #f]
              #:decode [decode-f #f]
              #:post-decode [post-decode-f #f])
  (define (bad)
    (error 'Wrap "cannot add encode/decode hook to non-base type\n  type: ~e\n  hook: ~e"
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
    #'(define name (Delay type.c))]))
