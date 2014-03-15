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
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/promise
         racket/match
         "private/base-types.rkt"
         "private/types.rkt"
         "private/der.rkt"
         "private/der-frame.rkt"
         "private/base256.rkt")
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
         BIT-STRING
         OCTET-STRING
         NULL
         OBJECT-IDENTIFIER
         PrintableString
         IA5String
         UTF8String

         OID
         build-OID

         define-asn1-type

         ;; private/base-types.rkt
         printable-string?
         ia5string?

         (contract-out
          ;; private/base-types.rkt
          [struct bit-string ([bytes bytes?] [unused (integer-in 0 7)])]

          ;; private/der.rkt
          [DER-encode-hooks
           (parameter/c
            (listof (cons/c asn1-type?
                            (or/c (list/c 'pre (-> any/c any/c))
                                  (list/c 'encode (-> any/c bytes?))))))]
          [DER-decode-hooks
           (parameter/c
            (listof (cons/c asn1-type?
                            (or/c (list/c 'decode (-> bytes? any/c))
                                  (list/c 'post (-> any/c any/c))))))]
          [DER-encode
           (-> asn1-type? any/c bytes?)]
          [DER-encode-value
           (-> asn1-type? any/c bytes?)]
          [DER-decode
           (-> asn1-type? bytes? any/c)]
          [DER-decode-value
           (-> asn1-type? bytes? any/c)]
          [DER-read
           (-> asn1-type? input-port? any/c)]
          ;; private/der-frame.rkt
          [struct DER-frame
                  ([tagclass (or/c 'universal 'application 'private 'context-specific)]
                   [tagkind (or/c 'primitive 'constructed)]
                   [tagnum exact-nonnegative-integer?]
                   [value bytes?])]
          [DER-frame->bytes
           (-> DER-frame? bytes?)]
          [bytes->DER-frame
           (-> bytes? DER-frame?)]
          [read-DER-frame
           (-> input-port? DER-frame?)]
          ))

;; ============================================================

(begin-for-syntax
 (define-splicing-syntax-class tag-class
   (pattern (~seq #:universal) #:with tclass #'universal)
   (pattern (~seq #:private)   #:with tclass #'private)
   (pattern (~seq #:application) #:with tclass #'application)
   (pattern (~seq) #:with tclass #'context-specific))
 (define-splicing-syntax-class tag
   (pattern (~seq :tag-class #:explicit etag:nat)
            #:with wrap-type #'asn1-type:explicit-tag
            #:with tag #''(tclass etag))
   (pattern (~seq :tag-class #:implicit itag:nat)
            #:with wrap-type #'values
            #:with tag #''(tclass itag))
   (pattern (~seq)
            #:with wrap-type #'values
            #:with tag #''#f))
 (define-splicing-syntax-class option-clause
   (pattern (~seq #:optional)
            #:with option #''(optional))
   (pattern (~seq #:default v:expr)
            #:with option #'(list 'default v))
   (pattern (~seq)
            #:with option #''#f))

 (define-syntax-class element
   #:attributes (name type.c et)
   (pattern [name:id :tag type :option-clause]
            #:declare type (expr/c #'asn1-type?)
            #:with et #'(element 'name tag (wrap-type type.c) option #f)))

 (define-syntax-class sequence-element
   #:attributes (name type.c et dep)
   (pattern [#:dependent name:id :tag type :option-clause]
            #:declare type (expr/c #'asn1-type?)
            #:with et #'(element 'name tag (wrap-type ANY) option #f)
            #:with dep #'type.c)
   (pattern :element
            #:with dep #'#f))

 (define (in-rprefixes lst)
   ;; Produces list same length as lst
   (let loop ([lst lst] [rprefix null])
     (cond [(pair? lst)
            (cons rprefix (loop (cdr lst) (cons (car lst) rprefix)))]
           [(null? lst)
            null]))))

(define-syntax Sequence
  (syntax-parser
   [(Sequence e:sequence-element ...)
    #`(asn1-type:sequence
       (check-sequence-types
        (list #,@(for/list ([prefix-names (in-rprefixes (syntax->list #'(e.name ...)))]
                            [e-et (syntax->list #'(e.et ...))]
                            [e-dep (syntax->list #'(e.dep ...))])
                   #`(add-refine #,e-et (wrap-refine #,prefix-names #,e-dep))))))]))

(define (add-refine elt refine)
  (match elt
    [(element name tag type option _)
     (element name tag type option refine)]))

(define-syntax wrap-refine
  (syntax-parser
   [(wrap-refine (name ...) #f)
    #'#f]
   [(wrap-refine (name ...) type)
    #'(lambda (lvs)
        (let ([name (cond [(assq 'name lvs) => cadr] [else #f])] ...)
          type))]))

(define-syntax Set
  (syntax-parser
   [(Set e:element ...)
    #'(asn1-type:set (check-set-types (list e.et ...)))]))

(define-syntax Choice
  (syntax-parser
   [(Choice e:element ...)
    #'(asn1-type:choice (check-choice-types (list e.et ...)))]))

(define-syntax Tag
  (syntax-parser
   [(Tag :tag-class #:explicit etag:nat type)
    #:declare type (expr/c #'asn1-type?)
    #'(make-tag-type '(tclass etag) (asn1-type:explicit-tag type.c))]
   [(Tag :tag-class #:implicit itag:nat type)
    #:declare type (expr/c #'asn1-type?)
    #'(make-tag-type '(tclass itag) type.c)]))

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
  (asn1-type:wrap type pre-encode-f encode-f decode-f post-decode-f))

;; ============================================================

(define ANY (asn1-type:any))
(define BOOLEAN (asn1-type:base 'BOOLEAN))
(define INTEGER (asn1-type:base 'INTEGER))
(define BIT-STRING (asn1-type:base 'BIT-STRING))
(define OCTET-STRING (asn1-type:base 'OCTET-STRING))
(define NULL (asn1-type:base 'NULL))
(define OBJECT-IDENTIFIER (asn1-type:base 'OBJECT-IDENTIFIER))
(define ENUMERATED (asn1-type:base 'ENUMERATED))
(define PrintableString (asn1-type:base 'PrintableString))
;; T61String
(define IA5String (asn1-type:base 'IA5String))
;; UTCTime
(define UTF8String (asn1-type:base 'UTF8String))

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
