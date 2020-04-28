#lang racket/base
(provide (all-defined-out))

;; Generic

(struct expr:dot (expr member) #:prefab)
(struct expr:apply (expr args) #:prefab)

(struct ast:named (name thing) #:prefab)

(struct tag (class number) #:prefab)


;; References

(struct ref:value (name) #:prefab)
(struct ref:type (name) #:prefab)
(struct ref:module (name) #:prefab)

(struct ref:object-class (name) #:prefab)
(struct ref:object (name) #:prefab)
(struct ref:object-set (name) #:prefab)

(struct ref:type-field (name) #:prefab)
(struct ref:value-field (name) #:prefab)
(struct ref:value-set-field (name) #:prefab)
(struct ref:object-field (name) #:prefab)
(struct ref:object-set-field (name) #:prefab)

;; Assignments

(struct assign:type (name params type) #:prefab)
(struct assign:value (name params type value) #:prefab)
(struct assign:value-set (name params type value-set) #:prefab)
(struct assign:class (name params class) #:prefab)
(struct assign:object (name params class object) #:prefab)
(struct assign:object-set (name params class object-set) #:prefab)

;; Types

(struct type:bit-string (named) #:prefab)
(struct type (name) #:prefab)
(struct type:choice (alts) #:prefab)
(struct type:enum (names) #:prefab)
(struct type:integer (names) #:prefab)
(struct type:sequence (fields) #:prefab)
(struct type:set (fields) #:prefab)
(struct type:set-of (type size-c) #:prefab)
(struct type:sequence-of (type size-c) #:prefab)
(struct type:string (subtype) #:prefab)
(struct type:tagged (tag mode type) #:prefab)
(struct type:constrained (type constraint) #:prefab)
(struct type:any-defined-by (id) #:prefab)
(struct type:from-object (object field) #:prefab)
(struct type:instance-of (oid) #:prefab)
(struct type:select (id type) #:prefab)

;; Values

(struct value:oid/reloid (components) #:prefab)
(struct value:choice (name value) #:prefab)
(struct value:seq/set-of (values) #:prefab)
(struct value:seq/set (values) #:prefab)
(struct value:from-object (object field) #:prefab)
(struct value:annotated (type value) #:prefab)

(struct named-value (name value) #:prefab)

(struct value-set:defn (values) #:prefab)
(struct value-set:from-object (object field) #:prefab)

;; Constraints

(struct constraint:and (c1 c2) #:prefab)
(struct constraint:or (c1 c2) #:prefab)
(struct constraint:single-value (value) #:prefab)
(struct constraint:includes (type) #:prefab)
(struct constraint:value-range (lo hi) #:prefab)
(struct constraint:size (c) #:prefab)
(struct constraint:user () #:prefab)

;; Classes

(struct class:defn (components stx) #:prefab)
(struct class:type-identifier () #:prefab)

(struct object:from-object (object field) #:prefab)
(struct object-set:from-object (object field) #:prefab)

(struct object:defn (decls) #:prefab)
(struct object:sugar (things) #:prefab)

(struct sugar:optional (things) #:prefab)
(struct sugar:literal (word) #:prefab)
(struct sugar:comma () #:prefab)
(struct sugar:reserved (word) #:prefab)

(struct object-set:defn (elems) #:prefab)

;; ----------------------------------------
(define fixme vector)
