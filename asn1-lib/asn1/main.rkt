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
         "private/types.rkt"
         "private/der.rkt"
         "private/base256.rkt")
(provide define-asn1-type
         Sequence
         SequenceOf
         Set
         SetOf
         Choice

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

         ;; private/der.rkt
         (contract-out
          [DER-encode-hooks
           (parameter/c
            (listof (list/c asn1-type? 'pre (-> asn1-type? any/c bytes?))))]
          [DER-decode-hooks
           (parameter/c
            (listof (list/c asn1-type? (or/c 'pre 'post) (-> asn1-type? bytes? any/c))))]
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
          ))

;; ============================================================

(define-syntax define-asn1-type
  (syntax-parser
   [(define-asn1-type name:id type)
    #:declare type (expr/c #'asn1-type?)
    #'(define name
        (asn1-type:defined 'name (delay type.c)))]))

(begin-for-syntax
 (define-splicing-syntax-class tag-class
   (pattern (~seq #:universal) #:with tclass #'universal)
   (pattern (~seq #:private)   #:with tclass #'private)
   (pattern (~seq #:application) #:with tclass #'application)
   (pattern (~seq) #:with tclass #'context-specific))
 (define-splicing-syntax-class option-clause
   (pattern (~seq #:optional)
            #:with option #''(optional))
   (pattern (~seq #:default v:expr)
            #:with option #'(list 'default v))
   (pattern (~seq)
            #:with option #''#f))

 (define-syntax-class element
   (pattern [name:id #:explicit etag:nat type :option-clause]
            #:declare type (expr/c #'asn1-type?)
            #:with et #'(element-type 'name '(implicit etag)
                                      (asn1-type:explicit-tag type.c)
                                      option))
   (pattern [name:id #:implicit itag:nat type :option-clause]
            #:declare type (expr/c #'asn1-type?)
            #:with et #'(element-type 'name '(implicit itag) type.c option))
   (pattern [name:id type :option-clause]
            #:declare type (expr/c #'asn1-type?)
            #:with et #'(element-type 'name '#f type.c option))))

(define-syntax Sequence
  (syntax-parser
   [(Sequence e:element ...)
    #'(asn1-type:sequence (check-sequence-types (list e.et ...)))]))

(define-syntax SequenceOf
  (syntax-parser
   [(SequenceOf type)
    #:declare type (expr/c #'asn1-type?)
    #'(asn1-type:sequence-of type.c)]))

(define-syntax Set
  (syntax-parser
   [(Set e:element ...)
    #'(asn1-type:set (check-set-types (list e.et ...)))]))

(define-syntax SetOf
  (syntax-parser
   [(SetOf type)
    #:declare type (expr/c #'asn1-type?)
    #'(asn1-type:set-of type.c)]))

(define-syntax Choice
  (syntax-parser
   [(Choice e:element ...)
    #'(asn1-type:choice (check-choice-types (list e.et ...)))]))

;; ============================================================

(define ANY (asn1-type:any))
(define BOOLEAN (asn1-type:base 'BOOLEAN))
(define INTEGER (asn1-type:base 'INTEGER))
(define BIT-STRING (asn1-type:base 'BIT-STRING))
(define OCTET-STRING (asn1-type:base 'OCTET-STRING))
(define NULL (asn1-type:base 'NULL))
(define OBJECT-IDENTIFIER (asn1-type:base 'OBJECT-IDENTIFIER))
(define PrintableString (asn1-type:base 'PrintableString))
;; T61String
(define IA5String (asn1-type:base 'IA5String))
;; UTCTime
(define UTF8String (asn1-type:base 'UTF8String))
