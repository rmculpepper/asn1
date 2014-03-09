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
(require racket/match
         racket/promise
         "base-types.rkt")
(provide (all-defined-out))

;; ASN.1 Types

;; Reference: http://luca.ntop.org/Teaching/Appunti/asn1.html

;; ============================================================

;; Asn1-Type is one of
;; - (asn1-type:base symbol)
;; - (asn1-type:any)
;; - (asn1-type:sequence (list Asn1-Element-Type ...))
;; - (asn1-type:sequence-of Asn1-Type)
;; - (asn1-type:set (list Asn1-Element-Type ...))
;; - (asn1-type:set-of Asn1-Type)
;; - (asn1-type:choice (list Asn1-Element-Type ...))
;; - (asn1-type:tag Tag Asn1-Type)
;; - (asn1-type:explicit-tag Asn1-Type)
;; - (asn1-type:wrap (Asn1-Type procedure/#f * 4))
;; - (asn1-type:delay (promiseof Asn1-Type))
(struct asn1-type () #:transparent)
(struct asn1-type:any asn1-type () #:transparent)
(struct asn1-type:base asn1-type (name) #:transparent)
(struct asn1-type:sequence asn1-type (elts) #:transparent)
(struct asn1-type:sequence-of asn1-type (elt) #:transparent)
(struct asn1-type:set asn1-type (elts) #:transparent)
(struct asn1-type:set-of asn1-type (elt) #:transparent)
(struct asn1-type:choice asn1-type (elts) #:transparent)
(struct asn1-type:tag asn1-type (tag type) #:transparent)
(struct asn1-type:explicit-tag asn1-type (type) #:transparent)
(struct asn1-type:wrap asn1-type (type pre-encode encode decode post-decode) #:transparent)
(struct asn1-type:delay asn1-type (promise))

;; Asn1-Element-Type is one of
;; - (element Symbol MaybeTag Asn1-Type MaybeOptionalDefault)
;; Desugars explicit tagging into asn1-type:explicit-tag.
(struct element-type (name tag type option) #:transparent)

;; MaybeTag is one of
;; - (list class nat)    -- implicit or desugared explicit
;; - #f

;; MaybeOptionalDefault is one of
;; - (list 'optional)
;; - (list 'default Value)
;; - #f

;; ----------------------------------------

(define (check-sequence-types ets)
  (check-elements 'Sequence ets)
  ;; All runs of optional components in a SEQUENCE must have distinct
  ;; tags, and their tags must be distinct from following required
  ;; component. (p 220)
  ;; FIXME
  ets)

(define (check-set-types ets)
  (check-elements 'Set ets)
  ;; All components of a SET must have distinct tags. (p226)
  (check-duplicate-types 'Set ets))

(define (check-choice-types ets)
  (check-elements 'Choice ets)
  ;; All components of a CHOICE must have distinct tags. (p236)
  (check-duplicate-types 'Choice ets))

(define (make-tag-type tag type)
  (check-can-tag 'Tag tag type)
  (asn1-type:tag tag type))

(define (check-can-tag who tag type)
  ;; Can't implicit-tag a CHOICE type.
  (when (and (pair? tag) (eq? (car tag) 'implicit))
    (match type
      [(asn1-type:any)
       (error who "indeterminate tag")]
      [(asn1-type:choice _)
       (error who "indeterminate tag")]
      [(asn1-type:delay promise)
       (check-can-tag who tag (force promise))]
      [_ (void)])))

(define (check-elements who ets)
  (for ([et (in-list ets)])
    (match et
      [(element-type name tag type _)
       (check-can-tag who tag type)])))

(define (check-duplicate-types who ets)
  (define tags (apply append (map type->tags ets)))
  (when (memq #f tags)
    ;; Do not allow indeterminate tag in CHOICE or SET
    (error who "indeterminate tag"))
  (cond [(let loop ([tags tags])
           (and (pair? tags)
                (or (member (car tags) (cdr tags))
                    (loop (cdr tags)))))
         (error who "duplicate tag: ~e" (car tags))])
  ets)

;; FIXME: references to defined types may cause force-cycle problems

;; type->tags : (U Asn1-Type Element-Type) -> (listof (U Tag #f))
;; #f means all possible tags; collides with everything
(define (type->tags t)
  (match t
    [(asn1-type:any) (list #f)]
    [(asn1-type:base base-type)
     (cond [(type->tag-entry base-type)
            => (lambda (te)
                 (list (list 'universal (tag-entry-tagn te))))]
           [else
            (error 'type->tags "unknown base type: ~e" base-type)])]
    [(asn1-type:sequence _)
     (list (tag-entry-tag (type->tag-entry 'SEQUENCE)))]
    [(asn1-type:sequence-of _)
     (list (tag-entry-tag (type->tag-entry 'SEQUENCE)))]
    [(asn1-type:set _)
     (list (tag-entry-tag (type->tag-entry 'SET)))]
    [(asn1-type:set-of _)
     (list (tag-entry-tag (type->tag-entry 'SET)))]
    [(asn1-type:choice elts)
     (apply append (map (type->tags elts)))]
    [(asn1-type:tag tag type)
     (list tag)]
    [(asn1-type:explicit-tag type)
     ;; should always be guarded by element-type w/ tag
     (error 'type->tags "internal error")]
    [(asn1-type:wrap type _ _ _ _)
     (type->tags type)]
    [(asn1-type:delay promise)
     (type->tags (force promise))]
    [(element-type name tag type option)
     (if tag
         (list tag)
         (type->tags type))]))
