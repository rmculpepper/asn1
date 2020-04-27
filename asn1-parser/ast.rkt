#lang racket/base
(provide (all-defined-out))

;; Assignments

(struct assign:type (name type) #:prefab)
(struct assign:value (name type value) #:prefab)

(struct assign:type-fun (name params type) #:prefab)
(struct assign:value-fun (name params type value) #:prefab)

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

;; Values

(struct value:oid/reloid (components) #:prefab)
(struct value:choice (name value) #:prefab)
(struct value:seq/set-of (values) #:prefab)
(struct value:seq/set (values) #:prefab)

(struct named-value (name value) #:prefab)

;; Constraints

(struct constraint:and (c1 c2) #:prefab)
(struct constraint:or (c1 c2) #:prefab)
(struct constraint:value (value) #:prefab)
(struct constraint:interval (lo hi) #:prefab)
(struct constraint:size (c) #:prefab)

(define fixme vector)
