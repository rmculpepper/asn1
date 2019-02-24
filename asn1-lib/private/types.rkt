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
(require racket/match
         racket/promise
         racket/struct
         "base.rkt")
(provide (all-defined-out))

;; ASN.1 Types

;; Reference: http://luca.ntop.org/Teaching/Appunti/asn1.html

;; ============================================================

;; Type is one of
;; - (asn1-type:any)
;; - (asn1-type:base Symbol)
;; - (asn1-type:sequence (list Component ...))
;; - (asn1-type:sequence-of Type)
;; - (asn1-type:set (list Component ...))
;; - (asn1-type:set-of Type)
;; - (asn1-type:choice (list Variant ...))
;; - (asn1-type:implicit-tag Tag Type)
;; - (asn1-type:explicit-tag Tag Type)
;; - (asn1-type:wrap Type procedure/#f * 2)
;; - (asn1-type:delay (promiseof Type))
(struct asn1-type ()
  #:property prop:custom-write (lambda (v out mode) (print-type v out mode))
  #:property prop:custom-print-quotable 'never)
(struct asn1-type:any asn1-type ())
(struct asn1-type:base asn1-type (name))
(struct asn1-type:sequence asn1-type (components))
(struct asn1-type:sequence-of asn1-type (elt))
(struct asn1-type:set asn1-type (components))
(struct asn1-type:set-of asn1-type (elt))
(struct asn1-type:choice asn1-type (variants))
(struct asn1-type:implicit-tag asn1-type (tag type))
(struct asn1-type:explicit-tag asn1-type (tag type))
(struct asn1-type:wrap asn1-type (type pre-encode post-decode))
(struct asn1-type:delay asn1-type (promise))

;; Custom printing for types
(define visited-types (make-parameter (hasheq)))
(define (literal s) (unquoted-printing-string (format "~a" s)))
(struct ppcons (c vs)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (v) (ppcons-c v)) (lambda (v) (ppcons-vs v))))
(define (print-type type p mode)
  (define (recur x) (case mode ((#t) (write x p)) ((#f) (display x p)) ((0 1) (print x p mode))))
  (define (for-tag tag)
    (define-values (class index) (tag->class+index tag))
    (cond [(eq? class 'context-specific) (list index)]
          [else (list (literal (string->keyword (symbol->string class))) index)]))
  (define for-component
    (match-lambda
      [(component name type #f #f tags) (ppcons name (list (for-type type)))]
      [(component name type _ _ _) (ppcons name (list (for-type type) (literal "...")))]))
  (define for-variant
    (match-lambda [(variant name type tags) (ppcons name (list (for-type type)))]))
  (define (for-type type)
    (match type
      [(asn1-type:any) (literal 'ANY)]
      [(asn1-type:base name) (literal name)]
      [(asn1-type:sequence components) (ppcons 'SEQUENCE (map for-component components))]
      [(asn1-type:sequence-of elt) (ppcons 'SEQUENCE-OF (list (for-type elt)))]
      [(asn1-type:set components) (ppcons 'SET (map for-component components))]
      [(asn1-type:set-of elt) (ppcons 'SET-OF (list (for-type elt)))]
      [(asn1-type:choice variants) (ppcons 'CHOICE (map for-variant variants))]
      [(asn1-type:implicit-tag tag type)
       (ppcons 'TAG `(,(literal "#:implicit") ,@(for-tag tag) ,(for-type type)))]
      [(asn1-type:explicit-tag tag type)
       (ppcons 'TAG `(,(literal "#:explicit") ,@(for-tag tag) ,(for-type type)))]
      [(asn1-type:wrap type pre-encode post-decode)
       (ppcons 'WRAP (list (for-type type) (literal "...")))]
      [(asn1-type:delay promise)
       (cond [(hash-ref (visited-types) type #f)
              (ppcons 'DELAY (list (literal "...")))]
             [else
              (parameterize ((visited-types (hash-set (visited-types) type #t)))
                (ppcons 'DELAY (list (for-type (force promise)))))])]
      [_ (literal "#<asn1-type>")]))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error 'print-asn1-type "internal error in type printer"))])
    (recur (for-type type))))

;; ----------------------------------------

(define (make-base-type base-type)
  (asn1-type:base base-type))

;; ----------------------------------------

;; A Component is (component Symbol Type MaybeOptional MaybeRefine (Listof Tag/#f))
;; - refine can only be non-#f in Sequence types
(struct component (name type option refine tags))

;; MaybeOptional is one of
;; - (list 'optional)
;; - (list 'default Value)
;; - #f

;; MaybeRefine is one of
;; - (Hash[Symbol => Any] -> Type)
;; - #f

;; make-component : Symbol Type MaybeOptional -> Component
(define (make-component name type option)
  (component name type option #f (type->tags type)))

;; component-add-refine : Component (Hash[Symbol=>Any] -> Type) -> Component
(define (component-add-refine ct refine)
  (match-define (component name type option _ tags) ct)
  (component name type option refine tags))

;; check-sequence-components : Symbol (Listof Component) -> (Listof Component)
(define (check-sequence-components who cts)
  ;; FIXME: All runs of optional components in a SEQUENCE must have
  ;; distinct tags, and their tags must be distinct from following
  ;; required component. (p 220)
  cts)

;; check-set-components : Symbol (Listof Component) -> (Listof Component)
(define (check-set-components who cts)
  ;; All components of a SET must have distinct tags. (p226)
  (check-duplicate-tag who (map component-name cts) (map component-type cts) "component")
  cts)

;; check-duplicate-tag : Symbol (Listof Symbol) (Listof Type) String -> Void or (error)
(define (check-duplicate-tag who names types label)
  (for/fold ([prev-tags (hash)]) ([name (in-list names)] [type (in-list types)])
    (for/fold ([prev-tags prev-tags]) ([tag (in-list (type->tags type))])
      (cond [(eq? tag #f)
             (error who "indeterminate tag\n  ~a: ~s" label name)]
            [(hash-ref prev-tags tag #f)
             => (lambda (prev-name)
                  (error who "duplicate tag\n  tag: ~a\n  1st ~a: ~s\n  2nd ~a: ~s"
                         (display-tag tag) label prev-name label name))]
            [else (void)])
      (hash-set prev-tags tag name)))
  (void))

;; ----------------------------------------

;; A Variant is (variant Symbol Type (U (Listof Tag) #f))
;; Tags is always list except for specially-constructed types, eg ANY*.
(struct variant (name type tags))

;; make-variant : Symbol Type -> Variant
(define (make-variant name type) (variant name type (type->tags type)))

;; check-choice-variants : Symbol Boolean (Listof Variant) -> (Listof Variant)
(define (check-choice-variants who allow-overlap? vs)
  ;; All components of a CHOICE must have distinct tags. (p236)
  (unless allow-overlap?
    (check-duplicate-tag who (map variant-name vs) (map variant-type vs) "variant"))
  vs)

;; variants-name-assq : Symbol (Listof Variant) -> Variant/#f
(define (variants-name-assq name vs)
  (for/or ([v (in-list vs)]) (and (eq? (variant-name v) name) v)))

;; variants-tag-assq : Tag (Listof Variant) -> Variant/#f
(define (variants-tag-assq tag vs)
  (for/or ([v (in-list vs)])
    (let ([vtags (variant-tags v)])
      (and (or (not vtags) (memq tag vtags)) v))))

;; ----------------------------------------

;; type-add-tag : Symbol Type (U 'explicit 'implicit #f) Tag -> Type
(define (type-add-tag who type mode tag [force? #f])
  (case mode
    [(implicit)
     (unless (or (can-implicit-tag? type) force?)
       (error who "cannot implicitly tag\n  type: ~e" type))
     (asn1-type:implicit-tag tag type)]
    [(explicit) (asn1-type:explicit-tag tag type)]
    [(#f) type]))

;; can-implicit-tag? : Type -> Boolean
;; Can't implicit-tag a CHOICE type, ANY, etc
(define (can-implicit-tag? type)
  (match type
    [(asn1-type:any) #f]
    [(asn1-type:choice _) #f]
    [(asn1-type:wrap type _ _) (can-implicit-tag? type)]
    [(asn1-type:delay promise) (can-implicit-tag? (force promise))]
    [_ #t]))

;; FIXME: references to defined types may cause force-cycle problems

;; ----------------------------------------

;; type->tags : Type -> (listof (U Tag #f))
;; #f means all possible tags; collides with everything
(define (type->tags t)
  (match t
    [(asn1-type:any) (list #f)]
    [(asn1-type:base name) (list (base-type-tag name))]
    [(asn1-type:sequence _) (list (base-type-tag 'SEQUENCE))]
    [(asn1-type:sequence-of _) (list (base-type-tag 'SEQUENCE))]
    [(asn1-type:set _) (list (base-type-tag 'SET))]
    [(asn1-type:set-of _) (list (base-type-tag 'SET))]
    [(asn1-type:choice vs)
     (apply append (for/list ([v (in-list vs)]) (type->tags (variant-type v))))]
    [(asn1-type:implicit-tag tag type) (list tag)]
    [(asn1-type:explicit-tag tag type) (list tag)]
    [(asn1-type:wrap type _ _) (type->tags type)]
    [(asn1-type:delay promise) (type->tags (force promise))]
    [else (error 'type->tags "unknown type: ~e" t)]))
