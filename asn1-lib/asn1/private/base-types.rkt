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

;; Base types and tags.

#lang racket/base
(provide type->tag-entry
         tagn->tag-entry
         tag-entry-type
         tag-entry-tagn
         tag-entry-p/c
         tag-entry-tag

         ia5string?
         printable-string?
         (struct-out bit-string))

;; Reference: http://luca.ntop.org/Teaching/Appunti/asn1.html

(define type-tag-classes
  '(universal application private context-specific))

(define type-tags
  '([BOOLEAN            1   primitive]
    [INTEGER            2   primitive]
    [BIT-STRING         3   primitive]
    [OCTET-STRING       4   primitive]
    [NULL               5   primitive]
    [OBJECT-IDENTIFIER  6   primitive]
    ;; [REAL               9   primitive] ;; Weird, prob. not worth the effort.
    ;; [ENUMERATED        10   primitive]
    ;; [RELATIVE-OID      13   primitive]
    [SEQUENCE          16   constructed]
    [SET               17   constructed]
    [PrintableString   19   primitive]
    ;; [T61String         20   primitive]
    [IA5String         22   primitive]
    ;; [UTCTime           23   primitive]

    ;; [UniversalString   28   primitive] ;; UCS4
    ;; [BMPString         30   primitive] ;; UCS2
    [UTF8String        12   primitive] ;; UTF8
    ;; [GeneralizedTime   24   primitive]
    ))

;; A Tag is (list TagClass TagNumber)
;; A CTag is (cons (U 'primitive 'constructed) Tag)

(define (type->tag-entry type)
  (for/or ([entry (in-list type-tags)])
    (and (eq? type (car entry)) entry)))

(define (tagn->tag-entry tagn)
  (for/or ([entry (in-list type-tags)])
    (and (equal? tagn (cadr entry)) entry)))

(define (tag-entry-type te) (car te))
(define (tag-entry-tagn te) (cadr te))
(define (tag-entry-p/c te) (caddr te))
(define (tag-entry-tag te) (list 'universal (tag-entry-tagn te)))

;; ----

;; ia5string? : Any -> Boolean
(define (ia5string? s)
  (and (string? s)
       (for/and ([c (in-string s)])
         (< (char->integer c) 256))))

;; printable-string? : Any -> Boolean
(define (printable-string? s)
  (and (string? s) (regexp-match? #rx"^[-a-zA-Z0-9 '()+,./:=?]*$" s)))

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
