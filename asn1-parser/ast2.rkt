#lang racket/base
(require racket/match)
(provide (all-defined-out))

;; ------------------------------------------------------------
;; 9.1 Assignments

;; Assignments = (Listof Assignment)
;; Assignment =
;; | (assign:type TypeRef Params Type)
;; | (assign:value ValueRef Params Type Value)
;; | (assign:value-set TypeRef Type Params ValueSet)
;; | (assign:class ClassRef Params Class)
;; | (assign:object ObjectRef DefinedObjectClass Params Object)
;; | (assign:object-set ObjectSetRef DefinedObjectClass Params ObjectSet)

;; Type = BuiltinType | ReferencedType | ConstrainedType
;; ReferencedType = DefinedType | UsefulType | SelectionType | TypeFromObject | ValueSetFromObjects

;; Value = BuiltinValue | ReferencedValue
;; ReferencedValue = DefinedValue | ValueFromObject

;; ------------------------------------------------------------
;; 9.2 Module structure

;; ModDefn = (mod:defn ModId TagMode ExtMode Exports Imports Assignments)
;; ModId = (mod:id ModRef (U ModOID DefinedValue #f)) -- no DefinedValue in defn id
;; ModOID = (Listof (U Ident Integer (ast:named Ident Integer)))
;; TagMode = 'explicit | 'implicit | 'automatic | #f
;; ExtMode = Boolean  -- extensibility implied?

;; Exports = 'all | (Listof ModSymbol)
;; Imports = (Listof Import)
;; Import = (mod:import (Listof ModSymbol) ModId)
;; ModSymbol = Reference

;; Reference = TypeRef | ValueRef | ClassRef | ObjectRef | ObjectSetRef

;; ------------------------------------------------------------
;; 9.3 Local and external references

;; DefinedType =
;; | (ref:dot ModRef TypeRef)
;; | TypeRef
;; | ParameterizedType
;; | ParameterizedValueSetType?? -- disabled?

;; DefinedValue = (ref:dot ModRef ValueRef) | ValueRef | ParameterizedValue
;; DefinedObjectClass = (ref:dot ModRef ClassRef | ObjectClassRef | UsefulObjectClassRef
;; DefinedObject = (ref:dot ModRef ObjectRef) | ObjectRef
;; DefinedObjectSet = (ref:dot ModRef ObjectSetRef) | ObjectSetRef

;; ------------------------------------------------------------
;; Types

;; BuiltinType =
;; | (type Symbol)  -- built-in type
;; | (type:bit-string NamedValues)
;; | (type:choice Alternatives)
;; | (type:enum NamedValues)
;; | (type:integer NamedValues)
;; | (type:sequence Components)
;; | (type:set Components)
;; | (type:sequence-of Type SizeConstraint)
;; | (type:set-of Type SizeConstraint)
;; | (type:string Symbol) ;; FIXME: Character-String ?
;; | (type:tagged Tag (U 'implicit 'explicit #f) Type)
;; | (type:any-defined-by Ident)
;; | ObjectClassFieldType

;; NamedValues = (Listof (ast:named Symbol Value))
;; Tag = (tag TagClass (U Integer DefinedValue))
;; TagClass = 'universal | 'application | 'private | 'context-sensitive

;; Component = BaseComponent | (opt:optional BaseComponent) | (opt:default BaseComponent Value)
;; BaseComponent = (ast:named Ident Type) | (fixme 'components-of Type)

;; Alternative = (ast:named Ident Type)

;; SelectionType = (type:select Ident Type)

;; ------------------------------------------------------------
;; Values

;; BuiltinValue =
;; | (value (U 'NULL Boolean Integer String))
;; | (value:bstring String) -- [01]*
;; | (value:hstring String) -- [0-9A-F]*
;; | (value:annotated Type Value)
;; | (value:bit-list (Listof Identifier))
;; | (value:choice Ident Value)
;; | (value:oid/reloid (U Integer (ast:named Ident Integer) DefinedValue))
;; | (value:seq/set-of (Listof Value))
;; | (value:seq/set (Listof (ast:named Ident Value)))

;; ------------------------------------------------------------
;; 13 Subtype constraints

;; ConstrainedType = (type:constrained Type Constraint)

;; SingleValue = (constraint:single-value Value)
;; ContainedSubtype = (constraint:includes Type)
;; ValueRange = (constraint:value-range LowerEndPoint UpperEndPoint)
;; LowerEndPoint = Value | 'MIN
;; UpperEndPoint = Value | 'MAX

;; SizeConstraint = (constraint:size Constraint)
;; PatternConstraint = (constraint:pattern Value)

;; InnerTypeConstraints = (constraint:inner-type (U Constraint (Listof TypeConstraint)))
;; TypeConstraint = (ast:named Ident ComponentConstraint)
;; ComponentConstraint = (constraint:component (U Constraint #f) PresenceConstraint)
;; PresenceConstraint = 'present | 'absent | 'optional | #f

;; ContentsConstraint
;; | (constraint:containing Type)
;; | (constraint:containing/encoded-by (U Type #f) Value)

;; ElementSet = Unions
;; Unions = Intersections | (constraint:or Unions Intersections)
;; Intersections = Elements | (constraint:and Intersections Elements)
;; Elements = SubtypeElements | ObjectSetElements | ElementSet

;; SubtypeElements =
;; | SingleValue
;; | ContainedSubtype
;; | ValueRange
;; | SizeConstraint
;; | InnerTypeConstraints
;; | PatternConstraint -- FIXME

;; Constraint = ElementSet | GeneralConstraint |
;; UserDefinedConstraint = (constraint:user)

;; ------------------------------------------------------------
;; 15 Info classes...

;; ObjectClass = DefinedObjectClass | ObjectClassDefn | ParameterizedObjectClass
;; ObjectClassDefn = (class:defn (Listof Field) WithSyntaxSpec)

;; Field =
;; | (field:type TypeFieldRef Opt-Type)
;; | (field:value/fixed-type ValueFieldRef Type Unique Opt-Value)
;; | (field:value/var-type ValueFieldRef ?? Opt-Value)
;; | (field:value-set/fixed-type ValueSetFieldRef Type Opt-ValueSet)
;; | (field:value-set/var-type ValueSetFieldRef ?? Opt-ValueSet)
;; | (field:object ObjectFieldRef DefinedObjectClass Opt-Object)
;; | (field:object-set ObjectSetFieldRef DefinedObjectClass Opt-ObjectSet)

;; Unique = 'unique | #f

;; Opt-<X> = (opt:optional) | (opt:default <X>) | #f

;; <X>FieldName = (cons (Listof (U ObjectFieldRef ObjectSetFieldRef)) <X>FieldRef)

;; Object = ObjectDefn | DefinedObject | ObjectFromObject | ParameterizedObject

;; ObjectDefn =
;; | (object:defn (Listof (ast:named <X>FieldRef <X>)))
;; | (object:sugar Sugar)

;; WithSyntaxSpec = (Listof TokenOrGroup)
;; TokenOrGroup = Literal | FieldReference | (sugar:optional WithSyntaxSpec)
;; Literal = (sugar:literal (U Symbol #\,))

;; Sugar = (Listof (U Literal Setting))
;; Setting = Type | Value | ValueSet | Object | ObjectSet

;; ObjectSet = (object-set:defn ElementSet)
;; ValueSet = (value-set:defn ElementSet)

;; ObjectSetElements =
;; | Object
;; | DefinedObjectSet
;; | ObjectSetFromObjects
;; | ParameterizedObjectSet

;; ValueFromObject = (value:from-object ReferencedObjects Value-FieldName)
;; ValueSetFromObjects = (value-set:from-object ReferencedObjects ValueSet-FieldName)
;; TypeFromObject = (type:from-object ReferencedObjects Type-FieldName)
;; ObjectFromObject = (object:from-object ReferencedObjects Object-FieldName)
;; ObjectSetFromObjects = (object-set:from-object ReferencedObjects ObjectSet-FieldName)

;; ReferencedObjects =
;; | DefinedObject
;; | DefinedObjectSet
;; | ParameterizedObject
;; | ParameterizedObjectSet

;; ObjectClassFieldType = (type:from-class DefinedObjectClass FieldName) -- a variant of Type

;; GeneralConstraint =
;; | UserDefinedConstraint
;; | TableConstraint
;; | ContentsConstraint

;; TableConstraint = (constraint:table ObjectSet (U #f AtNotation)
;; AtNotation = (cons (U '#:outer #:inner) (Listof Ident))

;; UsefulObjectClassRef = (class:type-identifier) ;; FIXME?
;; InstanceOfType = (type:instance-of DefinedObjectClass)

;; ------------------------------------------------------------
;; 17 Parameterization

;; Param = (param (U Governor #f) Reference)

;; Governor = Type | DefinedObjectClass | Reference

;; ParameterizedType = (expr:apply SimpleDefinedType (Listof ActualParameter))
;; ParameterizedValue = (expr:apply SimpleDefinedValue (Listof ActualParameter))
;; ParameterizedObjectClass = (expr:apply DefinedObjectClass (Listof ActualParameter))
;; ParameterizedObject = (expr:apply DefinedObject (Listof ActualParameter))
;; ParameterizedObjectSet = (expr:apply DefinedObjectSet (Listof ActualParameter))

;; SimpleDefined<X> = External<X>Ref | <X>Ref

;; ActualParameter = Type | Value | ValueSet | DefinedObjectClass | Object | ObjectSet

;; ============================================================

(define symbol-class-h (make-weak-hasheq))

(define (symbol-classify sym)
  (unless (hash-has-key? symbol-class-h sym)
    (hash-set! symbol-class-h sym
               (let ([s (symbol->string sym)])
                 (cond [(regexp-match? #rx"^[a-z]" s) 'id]
                       [(regexp-match? #rx"^[A-Z]" s) 'word]
                       [(regexp-match? #rx"^[&][a-z]" s) '&id]
                       [(regexp-match? #rx"^[&][A-Z]" s) '&word]
                       [else #| error |# #f]))))
  (hash-ref symbol-class-h sym))

(define (id? s)    (and (symbol? s) (eq? (symbol-classify s) 'id)))
(define (word? s)  (and (symbol? s) (eq? (symbol-classify s) 'word)))
(define (&id? s)   (and (symbol? s) (eq? (symbol-classify s) '&id)))
(define (&word? s) (and (symbol? s) (eq? (symbol-classify s) '&word)))

;; ============================================================
;; Structures

;; Module

(struct mod:defn (id tagmode extmode exports imports assignments) #:prefab)
(struct mod:id (name oid) #:prefab)
(struct mod:import (syms modid) #:prefab)

;; Generic

(struct expr:dot (expr member) #:prefab)
(struct expr:apply (expr args) #:prefab)

(struct ast:named (name thing) #:prefab)

(struct tag (class number) #:prefab)
(struct param (governor ref) #:prefab)

(struct opt:optional (thing) #:prefab)
(struct opt:default (thing default) #:prefab)

;; References

(struct ref:dot (modref ref) #:prefab)

;; Assignments

(struct assign:word (name params rhs) #:prefab)
(struct assign:id (name params kind rhs) #:prefab)
(struct assign:x-set (name params kind rhs) #:prefab)

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
(struct type:from-class (class field) #:prefab)
(struct type:instance-of (oid) #:prefab)
(struct type:select (id type) #:prefab)
(struct type:param () #:prefab)
(struct type:imported (name) #:prefab)

;; Values

(struct value (v) #:prefab)
(struct value:bstring (s) #:prefab)
(struct value:hstring (s) #:prefab)
(struct value:bit-list (bits) #:prefab)
(struct value:oid/reloid (components) #:prefab)
(struct value:choice (name value) #:prefab)
(struct value:seq/set-of (values) #:prefab)
(struct value:seq/set (values) #:prefab)
(struct value:from-object (object field) #:prefab)
(struct value:annotated (type value) #:prefab)

(struct x-set:defn (members) #:prefab)

(struct value-set:defn (values) #:prefab)
(struct value-set:from-object (object field) #:prefab)

;; Constraints

(struct constraint:and (c1 c2) #:prefab)
(struct constraint:or (c1 c2) #:prefab)
(struct constraint:single-value (value) #:prefab)
(struct constraint:includes (type) #:prefab)
(struct constraint:value-range (lo hi) #:prefab)
(struct constraint:size (c) #:prefab)
(struct constraint:pattern (value) #:prefab)
(struct constraint:user () #:prefab)
(struct constraint:inner-type (cs) #:prefab)
(struct constraint:component (vc presence) #:prefab)
(struct constraint:containing (type) #:prefab)
(struct constraint:containing/encoded-by (type value) #:prefab)

(struct constraint:table (objset at) #:prefab)

;; Classes

(struct class:defn (components stx) #:prefab)
(struct class:primitive (name) #:prefab)

(struct object:from-object (object field) #:prefab)
(struct object-set:from-object (object field) #:prefab)

(struct object:defn (decls) #:prefab)
(struct object:sugar (things) #:prefab)

(struct sugar:optional (things) #:prefab)
(struct sugar:literal (word) #:prefab)

(struct object-set:defn (elems) #:prefab)

(struct field:type (ref opt) #:prefab)
(struct field:value/fixed-type (ref type unique opt) #:prefab)
(struct field:value/var-type (ref type opt) #:prefab)
(struct field:value-set/fixed-type (ref type opt) #:prefab)
(struct field:value-set/var-type (ref type opt) #:prefab)
(struct field:object (ref class opt) #:prefab)
(struct field:object-set (ref class opt) #:prefab)

(struct field:&word (ref kind opt) #:prefab)
(struct field:&id (ref kind opt) #:prefab)

;; ----------------------------------------
(define fixme vector)

(struct ambiguous (vs) #:prefab)

;; ============================================================

;; ast-kind : Any -> (U 'type 'class 'value 'value-set 'object, 'object-set 'x-set #f)
(define (ast-kind v)
  (match v
    ;; Types
    [(type name) 'type]
    [(type:bit-string named) 'type]
    [(type:choice alts) 'type]
    [(type:enum names) 'type]
    [(type:integer names) 'type]
    [(type:sequence fields) 'type]
    [(type:set fields) 'type]
    [(type:set-of type size-c) 'type]
    [(type:sequence-of type size-c) 'type]
    [(type:string subtype) 'type]
    [(type:tagged tag mode type) 'type]
    [(type:constrained type constraint) 'type]
    [(type:any-defined-by id) 'type]
    [(type:from-object object field) 'type]
    [(type:from-class class field) 'type]
    [(type:instance-of oid) 'type]
    [(type:select id type) 'type]
    [(type:param) 'type]
    [(type:imported sym) 'type]
    ;; Values
    [(value v) 'value]
    [(value:bstring s) 'value]
    [(value:hstring s) 'value]
    [(value:bit-list bits) 'value]
    [(value:oid/reloid components) 'value]
    [(value:choice name value) 'value]
    [(value:seq/set-of values) 'value]
    [(value:seq/set values) 'value]
    [(value:from-object object field) 'value]
    [(value:annotated type value) 'value]
    ;; Value sets
    [(value-set:defn values) 'value-set]
    [(value-set:from-object object field) 'value-set]
    ;; Classes
    [(class:defn components stx) 'class]
    [(class:primitive name) 'class]
    ;; Objects
    [(object:defn decls) 'object]
    [(object:sugar things) 'object]
    [(object:from-object object field) 'object]
    ;; Object sets
    [(object-set:defn elems) 'object-set]
    [(object-set:from-object object field) 'object-set]
    ;; Unknown
    [(x-set:defn members) 'x-set]
    [_ #f]))

(define (join-mod header defs)
  (match-define (mod:defn id tagmode extmode exports imports _) header)
  (mod:defn id tagmode extmode exports imports defs))


;; ============================================================
;; Prelude

(define standard-types
  '(;; CharacterStringType
    BMPString
    GeneralString
    GraphicString
    IA5String
    ISO646String
    NumericString
    PrintableString
    TeletexString
    T61String
    UniversalString
    UTF8String
    VideotexString
    VisibleString
    ;; UsefulType
    GeneralizedTime
    UTCTime
    ObjectDescriptor
    ))

(define standard-classes
  (hasheq 'TYPE-IDENTIFIER
          (class:defn
           (list (field:value/fixed-type '&id (type 'OBJECT-IDENTIFIER) 'unique #f)
                 (field:type '&Type #f))
           `(&Type IDENTIFIED BY &id))
          'ABSTRACT-SYNTAX
          (class:defn
           (list (field:value/fixed-type '&id (type 'OBJECT-IDENTIFIER) #f #f)
                 (field:type '&Type #f)
                 (field:value/fixed-type '&property (type:bit-string null) #f #f))
           `(&Type IDENTIFIED BY &id ,(sugar:optional `(HAS PROPERTY &property))))))

(define base-env
  (let ([env (for/fold ([env (hasheq)]) ([(name def) (in-hash standard-classes)])
               (hash-set env name (cons 'class def)))])
    (for/fold ([env env]) ([name (in-list standard-types)])
      (hash-set env name (cons 'type (type name))))))

;; ============================================================
;; Post-disambiguation

(struct expr:fun (params body) #:prefab)

(define (maybe-fun params body)
  (if (pair? params) (expr:fun params body) body))

(struct assign:type (name type) #:prefab)
(struct assign:value (name type value) #:prefab)
(struct assign:value-set (name type value-set) #:prefab)
(struct assign:class (name class) #:prefab)
(struct assign:object (name class object) #:prefab)
(struct assign:object-set (name class object-set) #:prefab)

(struct value-set:one (elem) #:prefab)
(struct object-set:one (elem) #:prefab)
