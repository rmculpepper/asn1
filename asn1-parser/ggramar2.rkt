#lang racket/base
(require racket/match
         racket/list
         grrparse
         "tree-util.rkt"
         "gparser-util.rkt"
         "glexer.rkt"
         "ast2.rkt")
(provide (all-defined-out))

(define-nt-definers define-nt define-asn1-grammar)

(define-nt Number [(num) $1])

(define-nt Identifier [(id) $1])
(define-nt &Identifier [(amp-id) $1])

(define-nt Word [(word) $1] [(word-caps) $1])
(define-nt &Word [(amp-word) $1] [(amp-word-caps) $1])

(define-nt Reference [(Identifier) $1] [(Word) $1])
(define-nt FieldReference [(&Identifier) $1] [(&Word) $1])

(define-nt ValueReference [(Identifier) $1])
(define-nt ModuleReference [(Word) $1])
(define-nt TypeReference [(Word) $1])
(define-nt ObjectClassReference [(Word) $1])
(define-nt ObjectReference [(Identifier) $1])
(define-nt ObjectSetReference [(Word) $1])
(define-nt TypeFieldReference [(&Word) $1])
(define-nt ValueFieldReference [(&Identifier) $1])
(define-nt ValueSetFieldReference [(&Word) $1])
(define-nt ObjectFieldReference [(&Identifier) $1])
(define-nt ObjectSetFieldReference [(&Word) $1])

(define-nt ReservedWORD
  ;; Don't allow type names and value names as syntax literals
  [(ABSENT) 'ABSENT] #;[(ABSTRACT-SYNTAX) 'ABSTRACT-SYNTAX] [(ALL) 'ALL]
  [(APPLICATION) 'APPLICATION] [(AUTOMATIC) 'AUTOMATIC]
  [(BEGIN) 'BEGIN] [(BIT) 'BIT] #;[(BOOLEAN) 'BOOLEAN] [(BY) 'BY]
  [(CHARACTER) 'CHARACTER] [(CHOICE) 'CHOICE] [(CLASS) 'CLASS] [(COMPONENT) 'COMPONENT]
  [(COMPONENTS) 'COMPONENTS] [(CONSTRAINED) 'CONSTRAINED] [(CONTAINING) 'CONTAINING]
  [(DEFAULT) 'DEFAULT] [(DEFINITIONS) 'DEFINITIONS]
  [(EMBEDDED) 'EMBEDDED] [(ENCODED) 'ENCODED] [(END) 'END] [(ENUMERATED) 'ENUMERATED]
  [(EXCEPT) 'EXCEPT] [(EXPLICIT) 'EXPLICIT] [(EXPORTS) 'EXPORTS]
  [(EXTENSIBILITY) 'EXTENSIBILITY] [(EXTERNAL) 'EXTERNAL]
  #;[(FALSE) 'FALSE] [(FROM) 'FROM]
  [(IDENTIFIER) 'IDENTIFIER] [(IMPLICIT) 'IMPLICIT]
  [(IMPLIED) 'IMPLIED] [(IMPORTS) 'IMPORTS] [(INCLUDES) 'INCLUDES] [(INSTANCE) 'INSTANCE]
  #;[(INTEGER) 'INTEGER] [(INTERSECTION) 'INTERSECTION]
  [(MAX) 'MAX] [(MIN) 'MIN] [(MINUS-INFINITY) 'MINUS-INFINITY]
  #;[(NULL) 'NULL]
  [(OBJECT) 'OBJECT] [(OCTET) 'OCTET] [(OF) 'OF]
  [(OPTIONAL) 'OPTIONAL]
  [(PATTERN) 'PATTERN] [(PDV) 'PDV] #;[(PLUS-INFINITY) 'PLUS-INFINITY] [(PRESENT) 'PRESENT]
  [(PRIVATE) 'PRIVATE]
  #;[(REAL) 'REAL] #;[(RELATIVE-OID) 'RELATIVE-OID]
  [(SEQUENCE) 'SEQUENCE] [(SET) 'SET] [(SIZE) 'SIZE] [(STRING) 'STRING] [(SYNTAX) 'SYNTAX]
  [(TAGS) 'TAGS] #;[(TRUE) 'TRUE]
  #;[(TYPE-IDENTIFIER) 'TYPE-IDENTIFIER]
  [(UNION) 'UNION] [(UNIQUE) 'UNIQUE] [(UNIVERSAL) 'UNIVERSAL]
  [(WITH) 'WITH]

  #;[(ANY) 'ANY] [(DEFINED) 'DEFINED])


;; ============================================================

(define-syntax-rule (define-nt* NT* Elem #:post [Post ...])
  (define-nt NT*
    [() null]
    [([x Elem] Post ... [xs NT*]) (cons x xs)]))

(define-syntax-rule (define-nt*+ NT* NT+ Elem #:sep [Sep ...])
  (begin
    (define-nt NT*
      [() null]
      [([xs NT+]) xs])
    (define-nt+ NT+ Elem #:sep [Sep ...])))

(define-syntax-rule (define-nt+ NT+ Elem #:sep [Sep ...])
  (define-nt NT+
    [([x Elem]) (list x)]
    [([x Elem] Sep ... [xs NT+]) (cons x xs)]))

;; no info object, classes, etc

;; no extension alternative (only extensibility marker)
;; - delete SET, SEQUENCE, CHOICE ExtensionAdditionAlternatives

;; ============================================================
;; Merged grammar

;; ============================================================
;; 9.2 Module Structure (pdf 141)

(define-nt ModuleDefinition
  [(ModuleIdentifier DEFINITIONS TagDefault ExtensionDefault ASSIGN BEGIN ModuleBody END)
   (let ()
     (match-define (list imports exports assignments) $7)
     (mod:defn $1 $3 $4 imports exports assignments))])

(define-nt ModuleIdentifier
  [(ModuleReference DefinitiveIdentifier)
   (mod:id $1 $2)])

(define-nt DefinitiveIdentifier
  [(LBRACE DefinitiveObjectIdComponents+ RBRACE) $2]
  [() #f])

(define-nt+ DefinitiveObjectIdComponents+ DefinitiveObjectIdComponent #:sep [])

(define-nt DefinitiveObjectIdComponent
  [(Identifier) $1]
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (ast:named $1 $3)])

(define-nt TagDefault
  [(EXPLICIT TAGS) 'explicit]
  [(IMPLICIT TAGS) 'implicit]
  [(AUTOMATIC TAGS) 'automatic]
  [() 'explicit])

(define-nt ExtensionDefault
  [(EXTENSIBILITY IMPLIED) #t]
  [() #f])

(define-nt ModuleBody
  [(Exports Imports AssignmentList)
   (list $1 $2 $3)]
  [()
   (list null null null)])

(define-nt Exports
  [(EXPORTS SEMICOLON) null]
  [(EXPORTS SymbolsExported+ SEMICOLON) $2]
  [(EXPORTS ALL SEMICOLON) 'all]
  [() 'all])

(define-nt+ SymbolsExported+ Symbol #:sep [COMMA])

(define-nt Imports
  [(IMPORTS SymbolsImported SEMICOLON) $2]
  [() null])

(define-nt* SymbolsImported SymbolsFromModule #:post [])

(define-nt SymbolsFromModule
  [(Symbols+ FROM GlobalModuleReference) (mod:import $1 $3)])

(define-nt+ Symbols+ Symbol #:sep [COMMA])

(define-nt GlobalModuleReference
  [(ModuleReference AssignedIdentifier) (mod:id $1 $2)])

(define-nt AssignedIdentifier
  [(ObjectIdentifierValue) $1]
  [(DefinedValue) $1]
  [() #f])

(define-nt Symbol
  [(Reference) $1]
  [(Reference LBRACE RBRACE) $1])

;; ----------------------------------------
;; merged

(define-nt SimpleDefinedId
  [(Identifier) $1])
(define-nt DefinedIdOrApp
  [(SimpleDefinedId) $1]
  [(SimpleDefinedId ActualParameterList) (expr:apply $1 $2)])

(define-nt SimpleDefinedWord
  [(Word) $1])
(define-nt DefinedWordOrApp
  [(SimpleDefinedWord) $1]
  [(SimpleDefinedWord ActualParameterList) (expr:apply $1 $2)])

(define-nt SimpleDefinedRef
  [(SimpleDefinedId) $1] [(SimpleDefinedWord) $1])
(define-nt DefinedRefOrApp
  [(DefinedIdOrApp) $1] [(DefinedWordOrApp) $1])

(define-nt DefinedValue
  [(DefinedIdOrApp) $1])
(define-nt DefinedObject
  [(DefinedIdOrApp) $1])
(define-nt DefinedObjectSet
  [(SimpleDefinedWord) $1])
(define-nt DefinedObjectClass
  [(SimpleDefinedWord) $1])

(define-nt ReferencedObjects
  [(DefinedRefOrApp) $1])

;; ============================================================
;; 9.1 Assignments (p 106, pdf 134)

(define-nt* AssignmentList Assignment #:post [])

(define-nt Assignment
  [(Word ASSIGN BAG-Type+ObjectClass)
   ;; TypeReference ASSIGN Type
   ;; ObjectClassReference ASSIGN ObjectClass
   (assign:word $1 null $3)]
  [([id Identifier] [ty Type+DefinedObjectClass] ASSIGN [val BAG-Value+Object])
   ;; ValueReference Type ASSIGN Value
   ;; ObjectReference DefinedObjectClass ASSIGN Object
   (assign:id id null ty val)]
  [(Word Type+DefinedObjectClass ASSIGN BAG-ValueSet+ObjectSet)
   ;; TypeReference Type ASSIGN ValueSet
   ;; ObjectSetReference DefinedObjectClass ASSIGN ObjectSet
   (assign:x-set $1 null $2 $4)]
  ;; --- Parameterized assignments ---
  [(Word ParameterList ASSIGN BAG-Type+ObjectClass)
   ;; TypeReference ParameterList ASSIGN Type
   ;; ObjectClassReference ParameterList ASSIGN ObjectClass
   (assign:word $1 $2 $4)]
  [(Identifier ParameterList Type+DefinedObjectClass ASSIGN BAG-Value+Object)
   ;; ValueReference ParameterList Type ASSIGN Value
   ;; ObjectReference ParameterList DefinedObjectClass ASSIGN Object
   (assign:id $1 $2 $3 $5)]
  [(Word ParameterList Type+DefinedObjectClass ASSIGN BAG-ValueSet+ObjectSet)
   ;; TypeReference ParameterList Type ASSIGN ValueSet
   ;; ObjectSetReference ParameterList DefinedObjectClass ASSIGN ObjectSet
   (assign:x-set $1 $2 $3 $5)])

(define-nt BAG-Type+ObjectClass
  [(Type+ObjectClass) (action:collect $1)])
(define-nt BAG-Value+Object
  [(Value+Object) (action:collect $1)])
(define-nt BAG-ValueSet+ObjectSet
  [(ValueSet+ObjectSet) (action:collect $1)])

(define-nt Type+ObjectClass
  [(Type) $1]
  ;; ObjectClass \ Type
  ;;  X [(DefinedObjectClass) _] -- overlaps Type
  ;;  X [(DefinedObjectClass ActualParameterList) _] -- overlaps Type
  [(CLASS LBRACE FieldSpec+ RBRACE WithSyntaxSpec) (class:defn $3 $5)])

(define-nt Type+DefinedObjectClass
  [(Type) $1]
  #| DefinedObjectClass \ Type is empty |#)

(define-nt Value+Object
  [(Value) $1]
  ;; Object \ Value
  ;;  X [(DefinedObject) _]
  ;;  X [(DefinedObject ActualParameterList) _]
  ;;  X [(ReferencedObjects DOT idFieldName) _]
  ;; DefaultSyntax
  [(LBRACE FieldSetting* RBRACE) (object:defn $2)]
  ;; DefinedSyntax -- overlaps, but not subset of Value
  [(LBRACE DefinedSyntaxToken* RBRACE)
   ;; Disambiguate: assume that a DefinedSyntax notation must include
   ;; at least one all-caps literal symbol. FIXME: maybe better to
   ;; just check for overlap with reloid and set/seq notation?
   (define (WORD? v) (and (symbol? v) (not (regexp-match? #rx"[a-z]" (symbol->string v)))))
   (cond [(ormap WORD? $2) (action:collect (object:sugar $2))]
         [else (action:reject)])])

(define-nt Type+ValueSet+ObjectSet
  [(Type) $1]
  [(ValueSet+ObjectSet) $1])

(define-nt ValueSet+ObjectSet
  [(LBRACE RootElementSetSpec OptionalExtensionMarker RBRACE) (x-set:defn $2)]
  [(LBRACE ELLIPSIS RBRACE) (x-set:defn null)])

;; ============================================================
;; Type

(define-nt Type
  ;; ----------------------------------------
  [(ANY) (type 'ANY)]
  [(ANY DEFINED BY Identifier) (type:any-defined-by $4)]
  [(BIT STRING LBRACE NamedBit+ RBRACE) (type:bit-string $4)]
  [(BIT STRING) (type:bit-string null)]
  [(BOOLEAN) (type 'BOOLEAN)]
  [(CHARACTER STRING) (type 'CHARACTER-STRING)]
  [(CHOICE LBRACE AlternativeTypeLists RBRACE) (type:choice $3)]
  [(EMBEDDED PDV) (type 'EMBEDDED-PDV)]
  [(ENUMERATED LBRACE Enumerations RBRACE) (type:enum $3)]
  [(EXTERNAL) (type 'EXTERNAL)]
  [(INSTANCE OF DefinedObjectClass) (type:instance-of $3)]
  [(INTEGER LBRACE NamedNumber+ RBRACE) (type:integer $3)]
  [(INTEGER) (type:integer null)]
  [(NULL) (type 'NULL)]
  [(OBJECT IDENTIFIER) (type 'OBJECT-IDENTIFIER)]
  [(OCTET STRING) (type 'OCTET-STRING)]
  [(REAL) (type 'REAL)]
  [(RELATIVE-OID) (type 'RELATIVE-OID)]
  [(SEQUENCE LBRACE ComponentTypeLists RBRACE) (type:sequence $3)]
  [(SEQUENCE OF Type) (type:sequence-of $3 #f)]
  [(SEQUENCE SizeConstraint OF Type) (type:sequence-of $4 $2)] ;; from TypeWithConstraint
  [(SET LBRACE ComponentTypeLists RBRACE) (type:set $3)]
  [(SET OF Type) (type:set-of $3 #f)]
  [(SET SizeConstraint OF Type) (type:set-of $4 $2)] ;; from TypeWithConstraint

  ;; ----------------------------------------
  ;; DefinedType
  [(DefinedWordOrApp) $1]
  ;; ----------------------------------------
  ;; ConstrainedType
  [(Type BAG-Constraint) (type:constrained $1 $2)]
  ;; TypeWithConstraint
  #;[(SEQUENCE Constraint OF Type) ...]
  #;[(SET Constraint OF Type) ...]
  ;; ----------------------------------------
  ;; TaggedType
  [(Tag Type)           (type:tagged $1 #f        $2)]
  [(Tag IMPLICIT Type)  (type:tagged $1 'implicit $3)]
  [(Tag EXPLICIT Type)  (type:tagged $1 'explicit $3)]
  ;; ----------------------------------------
  [(Identifier LESSTHAN Type) (type:select $1 $3)] ;; SelectionType
  ;; ----------------------------------------
  ;; {Type,ValueSet}FromObject, ObjectClassFieldType (merged)
  [(DefinedRefOrApp DOT wordFieldName) (type:from-object $1 $3)]
  [(DefinedRefOrApp DOT idFieldName) (type:from-object $1 $3)]
  )

(define-nt BAG-Constraint
  [(Constraint) (action:collect $1)])

;; ============================================================
;; Value

(define-nt Value
  [(NULL) (value 'NULL)]
  [(TRUE) (value #t)]
  [(FALSE) (value #f)]
  [(Number) (value $1)]
  [(bstring) (value:bstring $1)] ;; BIT STRING, OCTET STRING
  [(hstring) (value:hstring $1)] ;; BIT STRING, OCTET STRING
  [(cstring) (value $1)] ;; character string types
  [(Identifier COLON Value) (value:choice $1 $3)]
  ;; [(LBRACE Identifier* RBRACE) (value:bit-list $2)] ;; BIT STRING; overlaps SE[QT]-OF
  [(LBRACE Value* RBRACE) (value:seq/set-of $2)]
  [(LBRACE NamedValue* RBRACE) (value:seq/set $2)]
  [(LBRACE GenOIDComponents+ RBRACE) (value:oid/reloid $2)] ;; = ObjectIdentifierValue
  ;; ----------------------------------------
  ;; ObjectClassField -- overlaps with ChoiceVoid
  ;; FIXME: maybe only used in eg Setting ???
  [(Type COLON Value) (value:annotated $1 $3)]
  ;; ----------------------------------------
  [(DefinedValue) $1]
  ;; ----------------------------------------
  ;; ValueFromObject
  [(ReferencedObjects DOT idFieldName) (value:from-object $1 $3)])

;; ============================================================
;; Class, Object, etc

(define-nt ObjectClass
  [(CLASS LBRACE FieldSpec+ RBRACE WithSyntaxSpec) (class:defn $3 $5)]
  [(DefinedObjectClass) $1]
  [(DefinedObjectClass ActualParameterList) (expr:apply $1 $2)])

(define-nt Object
  [(DefinedObject) $1]
  [(DefinedObject ActualParameterList) (expr:apply $1 $2)]
  ;; ObjectFromObject
  [(ReferencedObjects DOT idFieldName) (object:from-object $1 $3)]
  ;; DefaultSyntax
  [(LBRACE FieldSetting* RBRACE) (object:defn $2)]
  ;; DefinedSyntax
  [(LBRACE DefinedSyntaxToken* RBRACE) (action:collect (object:sugar $2))])


;; ============================================================
;; ???



;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; ============================================================
;; 9.3 Local and external references (pdf 146)

;; ============================================================
;; TYPES:

;; ----------------------------------------
;; 10 Basic types (pdf 155)

;; INTEGER
(begin
  (define-nt+ NamedNumber+ NamedNumber #:sep [COMMA])
  (define-nt NamedNumber
    [(Identifier LPAREN Number RPAREN) (ast:named $1 $3)]
    [(Identifier LPAREN DefinedValue RPAREN) (ast:named $1 $3)]))

;; ENUMERATED
(begin
  (define-nt Enumerations
    [(Enumeration) $1]
    [(Enumeration COMMA ELLIPSIS) $1]) ;; FIXME
  (define-nt+ Enumeration EnumerationItem #:sep [COMMA])
  (define-nt EnumerationItem
    [(Identifier) $1]
    [(NamedNumber) $1])
  (define-nt EnumeratedValue
    [(Identifier) $1]))

;; BIT STRING
(begin
  (define-nt+ NamedBit+ NamedBit #:sep [COMMA])
  (define-nt NamedBit
    [(Identifier LPAREN Number RPAREN)
     (ast:named $1 $3)]
    [(Identifier LPAREN DefinedValue RPAREN)
     (ast:named $1 $3)]))

;; ----------------------------------------
;; 11 Character string types (pdf 199)

;; ----------------------------------------
;; 12 Constructed types, tagging, etc (pdf 233)

;; SEQUENCE, SET
(begin
  (define-nt ComponentTypeLists
    [() null]
    [(RootComponentTypeList) $1]
    [(RootComponentTypeList OptionalExtensionMarker) $1])
  (define-nt RootComponentTypeList
    [(ComponentTypeList+) $1])
  (define-nt+ ComponentTypeList+ ComponentType #:sep [COMMA])
  (define-nt ComponentType
    [(NamedType) $1]
    [(NamedType OPTIONAL) (opt:optional $1)]
    [(NamedType DEFAULT Value) (opt:default $1 $3)]
    [(COMPONENTS OF Type) (fixme 'components-of $3)])
  (define-nt NamedType
    [(Identifier Type) (ast:named $1 $2)]))

;; CHOICE
(begin
  (define-nt AlternativeTypeLists
    [(RootAlternativeTypeList)
     $1]
    [(RootAlternativeTypeList OptionalExtensionMarker)
     $1])
  (define-nt RootAlternativeTypeList
    [(AlternativeTypeList) $1])
  (define-nt AlternativeTypeList
    [(NamedType+) $1])
  (define-nt+ NamedType+ NamedType #:sep [COMMA]))

;; CHOICE, SEQUENCE, SET
(define-nt OptionalExtensionMarker
  [(COMMA ELLIPSIS) #t]
  [() #f])

;; tagged types
(begin
  (define-nt Tag
    [(LBRACKET Class ClassNumber RBRACKET) (tag $2 $3)])
  (define-nt Class
    [(UNIVERSAL)   'universal]
    [(APPLICATION) 'application]
    [(PRIVATE)     'private]
    [()            'context-sensitive])
  (define-nt ClassNumber
    [(Number) $1]
    [(DefinedValue) $1]))

;; ============================================================
;; VALUES:

;; ----------------------------------------
;; 10 Basic types

(define-nt*+ Identifier* Identifier+ Identifier #:sep [COMMA])

(define-nt ObjectIdentifierValue
  [(LBRACE GenOIDComponents+ RBRACE) (value:oid/reloid $2)])

(define-nt+ GenOIDComponents+ GenOIDComponent #:sep [])

(define-nt GenOIDComponent
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (ast:named $1 $3)]
  ;; DefinedValue contains Identifier: OID/Rel-OID or INTEGER
  [(DefinedValue) $1])

;; ----------------------------------------
;; 12 Constructed types...

(define-nt*+ Value* Value+ Value #:sep [COMMA])

(define-nt*+ NamedValue* NamedValue+ NamedValue #:sep [COMMA])

(define-nt NamedValue
  [(Identifier Value) (ast:named $1 $2)])

;; ============================================================
;; 13 Subtype constraints (pdf 285)

(define-nt SingleValue
  ;; SingleValue constraint overlaps *SetReference
  [(Value) (if (symbol? $1) $1 (constraint:single-value $1))])

(define-nt ContainedSubtype
  ;; INCLUDES was mandatory in 1990, optional since :(
  [(INCLUDES Type) (constraint:includes $2)]
  #;[(Type) (constraint:includes $1)])

(define-nt ValueRange
  [(LowerEndPoint DOTDOT UpperEndPoint)
   (constraint:value-range $1 $3)])

(define-nt LowerEndPoint
  [(LowerEndValue) $1]
  #;[(LowerEndValue LESSTHAN) ??])

(define-nt UpperEndPoint
  [(UpperEndValue) $1]
  #;[(LESSTHAN UpperEndValue) ??])

(define-nt LowerEndValue
  [(Value) $1]
  [(MIN) 'MIN])

(define-nt UpperEndValue
  [(Value) $1]
  [(MAX) 'MAX])

(define-nt SizeConstraint
  [(SIZE Constraint) (constraint:size $2)])

;; PermittedAlphabet

(define-nt PatternConstraint
  [(PATTERN Value) (constraint:pattern $2)])

(define-nt InnerTypeConstraints
  [(WITH COMPONENT SingleTypeConstraint) (constraint:inner-type $3)]
  [(WITH COMPONENTS MultipleTypeConstraints) (constraint:inner-type $3)])
(define-nt SingleTypeConstraint
  [(Constraint) $1])
(define-nt MultipleTypeConstraints
  [(FullSpecification) $1]
  [(PartialSpecification) $1])
(define-nt FullSpecification
  [(LBRACE TypeConstraints RBRACE) $2])
(define-nt PartialSpecification
  [(LBRACE ELLIPSIS COMMA TypeConstraints RBRACE) $4])
(define-nt TypeConstraints
  [(NamedConstraint+) $1])
(define-nt+ NamedConstraint+ NamedConstraint #:sep [COMMA])
(define-nt NamedConstraint
  [(Identifier ComponentConstraint) (ast:named $1 $2)])
(define-nt ComponentConstraint
  [(ValueConstraint PresenceConstraint) (constraint:component $1 $2)])
(define-nt ValueConstraint
  [(Constraint) $1]
  [() #f])
(define-nt PresenceConstraint
  [(PRESENT) 'present] [(ABSENT) 'absent] [(OPTIONAL) 'optional] [() #f])

(define-nt ContentsConstraint
  [(CONTAINING Type) (constraint:containing $2)]
  [(ENCODED BY Value) (constraint:containing/encoded-by #f $3)]
  [(CONTAINING Type ENCODED BY Value) (constraint:containing/encoded-by $2 $5)])

;; 13.11 Constraint combinations

(define-nt ElementSetSpecs
  [(RootElementSetSpec OptionalExtensionMarker) $1])

(define-nt RootElementSetSpec
  [(ElementSetSpec) $1])

(define-nt ElementSetSpec
  [(Unions) $1]
  #;[(ALL Exclusions) ??])

(define-nt Unions
  [(Intersections) $1]
  [(Unions UnionMark Intersections) (constraint:or $1 $3)])

(define-nt UnionMark
  [(UNION) #t]
  [(PIPE) #t])

(define-nt Intersections
  [(IntersectionElements) $1]
  [(Intersections IntersectionMark IntersectionElements) (constraint:and $1 $3)])

(define-nt IntersectionMark
  [(INTERSECTION) #t]
  [(CARET) #t])

(define-nt IntersectionElements
  [(Elements) $1]
  #;[(Elements Exclusions) ??])

(define-nt Elements
  [(SubtypeElements) $1]
  [(ObjectSetElements) $1] ;; not for subtype constraints
  [(LPAREN ElementSetSpec RPAREN) $2])

(define-nt SubtypeElements
  [(SingleValue) $1]
  [(ContainedSubtype) $1]
  [(ValueRange) $1]
  #;[(PermittedAlphabet) $1]
  [(SizeConstraint) $1]
  #;[(TypeConstraint) $1]
  [(InnerTypeConstraints) $1]
  [(PatternConstraint) $1])

;; 13.12 Constraint extensibility

(define-nt Constraint
  [(LPAREN ConstraintSpec RPAREN) $2])

(define-nt ConstraintSpec
  [(ElementSetSpecs) $1]
  [(GeneralConstraint) $1])

;; 13.13 User-defined constraint

(define-nt UserDefinedConstraint
  [(CONSTRAINED BY LBRACE UserDefinedConstraintParameter* RBRACE)
   (constraint:user)])

(define-nt*+ UserDefinedConstraintParameter* UserDefinedConstraintParameter+
  UserDefinedConstraintParameter #:sep [COMMA])

(define-nt UserDefinedConstraintParameter
  [(Governor COLON Value) #f]
  [(Governor COLON ValueSet) #f]
  [(Governor COLON Object) #f]
  [(Governor COLON ObjectSet) #f]
  [(Type) #t]
  [(DefinedObjectClass) #f])

;; ============================================================
;; 14 Presentation context switching types (pdf 325) (skipped)

;; EXTERNAL, EMBEDDED PDV, CHARACTER STRING

;; ============================================================
;; 15 Info classes... (pdf 337)


(define-nt+ FieldSpec+ FieldSpec #:sep [COMMA])

(define-nt FieldSpec
  [(ValueFieldReference Type UNIQUE ValueOptionalitySpec)
   (field:value/fixed-type $1 $2 'unique $4)]
  ;; ----
  [(TypeFieldReference TypeOptionalitySpec) (field:type $1 $2)]
  [(&Word Type+DefinedObjectClass ValueSet+ObjectSetOptionalitySpec)
   ;; [(ValueSetFieldReference Type ValueSetOptionalitySpec) _]
   ;; [(ObjectSetFieldReference DefinedObjectClass ObjectSetOptionalitySpec) _]
   (field:&word $1 $2 $3)]
  [(ValueFieldReference wordFieldName ValueOptionalitySpec)
   (field:value/var-type $1 $2 $3)]
  [(ValueSetFieldReference wordFieldName ValueSetOptionalitySpec)
   (field:value-set/var-type $1 $2 $3)]
  [(&Identifier Type+DefinedObjectClass Value+ObjectOptionalitySpec)
   ;; [(ValueFieldReference Type ValueOptionalitySpec) _]
   ;; [(ObjectFieldReference DefinedObjectClass ObjectOptionalitySpec) _]
   (field:&id $1 $2 $3)])

(define-nt ValueOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Value) (opt:default #f $2)] [() #f])
(define-nt TypeOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Type) (opt:default #f $2)] [() #f])
(define-nt ValueSetOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT ValueSet) (opt:default #f $2)] [() #f])
(define-nt Value+ObjectOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Value+Object) (opt:default #f $2)] [() #f])
(define-nt ValueSet+ObjectSetOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT ValueSet+ObjectSet) (opt:default #f $2)] [() #f])

(begin ;; DEPRECATED: imprecise
  (define-nt FieldName
    [(PrimitiveFieldName+) $1])
  (define-nt+ PrimitiveFieldName+ PrimitiveFieldName #:sep [DOT])
  (define-nt PrimitiveFieldName [(FieldReference) $1]))

(define-nt* FieldReference* FieldReference #:post [DOT])
(define-nt wordFieldName
  [(FieldReference* &Word) (append $1 (list $2))])
(define-nt idFieldName
  [(FieldReference* &Identifier) (append $1 (list $2))])

(define-nt*+ FieldSetting* FieldSetting+ FieldSetting #:sep [COMMA])

(define-nt FieldSetting
  [(&Identifier Value+Object) (ast:named $1 $2)]
  [(&Word Type+ValueSet+ObjectSet) (ast:named $1 $2)])

(define-nt Setting
  [(Value+Object) $1]
  [(Type+ValueSet+ObjectSet) $1])

;; IMPRECISE
;; (define-nt FieldSetting
;;   [(PrimitiveFieldName Setting) (fixme $1 $2)])
;; (define-nt Setting
;;   [(Type) $1]
;;   [(Value) $1]
;;   [(ValueSet) $1]
;;   [(Object) $1]
;;   [(ObjectSet) $1])

;; 15.3 User-friendly syntax

(define-nt WithSyntaxSpec
  [(WITH SYNTAX SyntaxList) $3]
  [() #f])

(define-nt SyntaxList
  [(LBRACE TokenOrGroupSpec+ RBRACE) $2])

(define-nt+ TokenOrGroupSpec+ TokenOrGroupSpec #:sep [])

(define-nt TokenOrGroupSpec
  [(RequiredToken) $1]
  [(OptionalGroup) $1])

(define-nt RequiredToken
  [(Literal) $1]
  [(PrimitiveFieldName) $1])

(define-nt OptionalGroup
  [(LBRACKET TokenOrGroupSpec+ RBRACKET) (sugar:optional $2)])

(define-nt Literal
  [(word-caps) $1]
  [(ReservedWORD) $1]
  [(COMMA) #\,])

(define-nt* DefinedSyntaxToken* DefinedSyntaxToken #:post [])

(define-nt DefinedSyntaxToken
  [([s Setting])
   (match s
     #;[(value 'NULL) ;; also gets parsed as (type 'NULL)
      (action:reject)]
     [_ s])]
  ;; Literal overlaps with Setting on word-caps < Word < TypeRef
  [(ReservedWORD) $1]
  [(COMMA) #\,])

;; 15.5 Value sets and information object sets (pdf 357)

(define-nt ObjectSet
  [(LBRACE ObjectSetSpec RBRACE)
   (object-set:defn $2)])

(define-nt ObjectSetSpec
  [(RootElementSetSpec OptionalExtensionMarker) $1]
  [(ELLIPSIS) null])

(define-nt ValueSet
  [(LBRACE ElementSetSpecs RBRACE) (value-set:defn $2)])

(define-nt ObjectSetElements
  [(Object) $1]
  [(DefinedWordOrApp) $1]
  [(ReferencedObjects DOT wordFieldName) (object-set:from-object $1 $3)])

;; 15.6 Accessing the information stored in objects and object sets (pdf 364)

;; 15.7 (pdf 369)

;; (define-nt ObjectClassFieldValue
;;   [(OpenTypeFieldVal) $1]
;;   [(FixedTypeFieldVal) $1])
;; (define-nt OpenTypeFieldVal
;;   [(Type COLON Value) (value:annotated $1 $3)]
;;   [(FixedTypeFieldVal) $1])
;; (define-nt FixedTypeFieldVal
;;   [(Value) $1])

(define-nt GeneralConstraint
  [(UserDefinedConstraint) $1]
  [(TableConstraint) $1]
  [(ContentsConstraint) $1])

(define-nt TableConstraint
  [(SimpleTableConstraint) $1]
  [(ComponentRelationConstraint) $1])

(define-nt SimpleTableConstraint
  [(ObjectSet) (constraint:table $1 null)])

(define-nt ComponentRelationConstraint
  [(LBRACE DefinedObjectSet RBRACE LBRACE AtNotation+ RBRACE)
   (constraint:table $2 $5)])

(define-nt+ AtNotation+ AtNotation #:sep [COMMA])

(define-nt AtNotation
  [(AT ComponentIdList) (cons '#:outer $2)]
  [(AT DOT ComponentIdList) (cons '#:inner $3)])

(define-nt ComponentIdList
  [(Identifier+) $1])

(define-nt+ Identifier.+ Identifier #:sep [DOT])

;; (define-nt TypeConstraint
;;   [(Type) $1])

;; 15.9 (pdf 383)

;; TYPE-IDENTIFIER ::= CLASS { &id OBJECT IDENTIFIER UNIQUE, &Type }
;; WITH SYNTAX { &Type IDENTIFIED BY id } -- ??? id, not &id ???

;; 15.10 ABSTRACT-SYNTAX (skip)

;; ============================================================
;; 16 Macros (skip)

;; ============================================================
;; 17 Paramerization (pdf 405)

(define-nt ParameterList
  [(LBRACE Parameter+ RBRACE) $2])

(define-nt+ Parameter+ Parameter #:sep [COMMA])

(define-nt Parameter
  [(ParamGovernor COLON Reference) (param $1 $3)]
  [(Reference) (param #f $1)])
(define-nt ParamGovernor
  ;; [(Reference) _] -- overlaps with Type
  [(Governor) $1])
(define-nt Governor
  [(Type+DefinedObjectClass) $1])

(define-nt ActualParameterList
  [(LBRACE ActualParameter+ RBRACE) (action:collect $2)])

(define-nt+ ActualParameter+ ActualParameter #:sep [COMMA])

(define-nt ActualParameter
  [(Value+Object) $1]
  [(Type+ValueSet+ObjectSet) $1]
  #;[(DefinedObjectClass) $1])

;; ============================================================
;; Incremental NTs

(define-nt ModuleHeader
  [(ModuleIdentifier DEFINITIONS TagDefault ExtensionDefault ASSIGN BEGIN ModuleBodyHeader)
   (let ()
     (match-define (list imports exports) $7)
     (mod:defn $1 $3 $4 imports exports null))])

(define-nt ModuleBodyHeader
  [(Exports Imports)
   (list $1 $2)])

(define-nt AssignmentOrEnd
  [(Assignment) $1]
  [(END) #f])

;; ============================================================
(require racket/class)

(define-asn1-grammar asn1-grammar)

(define asn1-module-parser
  (lr-parser #:slr1
             #:grammar asn1-grammar
             #:start ModuleDefinition
             #:end (EOF)))

(define asn1-module-header-parser
  (lr-parser #:grammar asn1-grammar
             #:start ModuleHeader
             #:end (id word word-caps END)))

(define asn1-assignment-parser
  (lr-parser #:grammar asn1-grammar
             #:start AssignmentOrEnd
             #:end (id word word-caps END EOF)))

;; ============================================================

(define (simplify-collect-boxes v)
  (define (simplify v)
    (cond [(collect-box? v)
           (define subvs
             (remove-duplicates
              (map simplify-collect-boxes (collect-box-contents v))))
           (cond [(= 1 (length subvs)) (car subvs)]
                 [else (ambiguous subvs)])]
          [else v]))
  (tree-transform v simplify))

(module+ main
  (require racket/pretty
           racket/cmdline)
  #;(define the-parser asn1-module-parser)
  (command-line
   #:once-any
   #;[("-a") "Parse an assignment list" (set! the-parser asn1-assignments-parser)]
   #;[("-m") "Parse a single module (default)" (void)]
   #:args files
   (for ([file files])
     (call-with-input-file* file
       (lambda (in)
         (port-count-lines! in)
         (pretty-print
          (remove-duplicates
           (map simplify-collect-boxes
                (send asn1-module-parser parse* (asn1-lexer in)))))
         #|
         (pretty-print
          (send asn1-module-header-parser parse* (asn1-lexer in)))
         (let loop ()
           (define rs (send asn1-assignment-parser parse* (asn1-lexer in)))
           (define rs* (remove-duplicates (map simplify-collect-boxes rs)))
           (define drs (filter (match-lambda [(token _ v) (ok-definition? v)]) rs*))
           (printf "\n-- ~s => ~s => ~s --\n" (length rs) (length rs*) (length drs))
           (match drs
             [(list (token 'AssignmentOrEnd defn))
              (process-definition defn)]
             [_ (void)])
           (when (> (length drs) 1)
             (printf "Ready?")
             (void (read-line)))
           (pretty-print drs)
           (unless (for/and ([r (in-list rs*)]) (eq? (token-value r) #f))
             (loop)))
         |#)))))
