#lang racket/base
(require racket/match
         grrparse
         "gparser-util.rkt"
         "glexer.rkt"
         "ast.rkt")
#;(provide asn1-module-parser
           asn1-assignments-parser)

(define-nt-definers define-nt define-asn1-grammar)

(define-nt WORD [(word-caps) $1])

(define-nt Word [(word) $1] [(word-caps) $1])
(define-nt &Word [(amp-word) $1])

(define-nt Identifier [(id) $1])
(define-nt &Identifier [(amp-id) $1])

(define-nt Reference [(Identifier) $1] [(Word) $1])
(define-nt FieldReference [(&Identifier) $1] [(&Word) $1])

(define-nt Number [(num) $1])

(define-nt ValueReference [(id) $1])
(define-nt ModuleReference [(Word) $1])
(define-nt TypeReference [(Word) $1])

(define-nt ObjectClassReference [(WORD) $1])
(define-nt ObjectReference [(id) $1])
(define-nt ObjectSetReference [(Word) $1])

(define-nt TypeFieldReference [(&Word) $1])
(define-nt ValueFieldReference [(&Identifier) $1])
(define-nt ValueSetFieldReference [(&Word) $1])
(define-nt ObjectFieldReference [(&Identifier) $1])
(define-nt ObjectSetFieldReference [(&Word) $1])

(define-nt ReservedWORD
  ;; Don't allow type names and value names as syntax literals
  [(ABSENT) 'ABSENT] [(ABSTRACT-SYNTAX) 'ABSTRACT-SYNTAX] [(ALL) 'ALL]
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
  [(TYPE-IDENTIFIER) 'TYPE-IDENTIFIER]
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
;; 9.1 Assignments (p 106, pdf 134)

(define-nt* AssignmentList Assignment #:post [])

(define-nt Assignment
  [(Word ASSIGN Type)
   (assign:type $1 null $3)]
  [(Identifier Type ASSIGN Value)
   (assign:value $1 null $2 $4)]
  [(Word Type ASSIGN ValueSet)
   (assign:value-set $1 null $2 $4)]
  [(WORD ASSIGN ObjectClass)
   (assign:class $1 null $3)]
  [(Identifier DefinedObjectClass ASSIGN Object)
   (assign:object $1 null $2 $4)]
  [(Word DefinedObjectClass ASSIGN ObjectSet)
   (assign:object-set $1 null $2 $4)]
  [(ParameterizedAssignment) $1])

(define-nt Type
  [(BuiltinType) $1]
  [(DefinedType) $1]
  [(UsefulType) $1]
  [(SelectionType) $1]
  [(TypeFromObject) $1]
  [(ValueSetFromObjects) $1]
  [(ConstrainedType) $1])

(define-nt Value
  [(BuiltinValue) $1]
  [(DefinedValue) $1]
  [(ValueFromObject) $1])

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
   (list $1 $2 $3)])

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
  [(ParameterizedReference) $1])

(define-nt ParameterizedReference
  #;[(Reference) $1] ;; redundant
  [(Reference LBRACE RBRACE) $1])

;; ============================================================
;; 9.3 Local and external references (pdf 146)

(define-nt DefinedType
  [(ExternalTypeReference) $1]
  [(TypeReference) $1]
  [(ParameterizedType) $1]
  #;[(ParameterizedValueSetType) $1])

(define-nt ExternalTypeReference
  [(ModuleReference DOT TypeReference) (ref:dot $1 $3)])

(define-nt DefinedValue
  [(ExternalValueReference) $1]
  [(ValueReference) $1]
  [(ParameterizedValue) $1])

(define-nt ExternalValueReference
  [(ModuleReference DOT ValueReference) (ref:dot $1 $3)])

(define-nt DefinedObjectClass
  [(ExternalObjectClassReference) $1]
  [(ObjectClassReference) $1]
  [(UsefulObjectClassReference) $1])

(define-nt ExternalObjectClassReference
  [(ModuleReference DOT ObjectClassReference) (ref:dot $1 $3)])

(define-nt DefinedObject
  [(ExternalObjectReference) $1]
  [(ObjectReference) $1])

(define-nt ExternalObjectReference
  [(ModuleReference DOT ObjectReference) (ref:dot $1 $3)])

(define-nt DefinedObjectSet
  [(ExternalObjectSetReference) $1]
  [(ObjectSetReference) $1])

(define-nt ExternalObjectSetReference
  [(ModuleReference DOT ObjectSetReference) (ref:dot $1 $3)])

;; ============================================================
;; TYPES:

(define-nt BuiltinType
  [(BIT STRING LBRACE NamedBit+ RBRACE) (type:bit-string $4)]
  [(BIT STRING) (type:bit-string null)]
  [(BOOLEAN) (type 'BOOLEAN)]
  [(CHOICE LBRACE AlternativeTypeLists RBRACE) (type:choice $3)]
  [(ENUMERATED LBRACE Enumerations RBRACE) (type:enum $3)]
  [(INTEGER LBRACE NamedNumber+ RBRACE) (type:integer $3)]
  [(INTEGER) (type:integer null)]
  [(NULL) (type 'NULL)]
  [(OBJECT IDENTIFIER) (type 'OBJECT-IDENTIFIER)]
  [(OCTET STRING) (type 'OCTET-STRING)]
  [(RELATIVE-OID) (type 'RELATIVE-OID)]
  [(SEQUENCE LBRACE ComponentTypeLists RBRACE) (type:sequence $3)]
  [(SEQUENCE OF Type) (type:sequence-of $3 #f)]
  [(SET LBRACE ComponentTypeLists RBRACE) (type:set $3)]
  [(SET OF Type) (type:set-of $3 #f)]
  ;; ----
  ;;[(EmbeddedPDVType) $1]
  ;;[(ExternalType) $1]
  [(ObjectClassFieldType) $1]
  [(InstanceOfType) $1]
  ;;[(RealType) $1]
  [(CharacterStringType) $1]
  [(TaggedType) $1]
  ;; ----------------------------------------
  [(ANY) (type 'ANY)]
  [(ANY DEFINED BY Identifier) (type:any-defined-by $4)])

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

;; FIXME: un-reserve these names, just parse them as TypeReferences ?

(define-nt CharacterStringType
  ;; RestrictedCharacterStringType
  [(BMPString)          (type:string 'BMPString)]
  [(GeneralString)      (type:string 'GeneralString)]
  [(GraphicString)      (type:string 'GraphicString)]
  [(IA5String)          (type:string 'IA5String)]
  [(ISO646String)       (type:string 'ISO656String)]
  [(NumericString)      (type:string 'NumericString)]
  [(PrintableString)    (type:string 'PrintableString)]
  [(TeletexString)      (type:string 'TeletexString)]
  [(T61String)          (type:string 'T61String)]
  [(UniversalString)    (type:string 'UniversalString)]
  [(UTF8String)         (type:string 'UTF8String)]
  [(VideotexString)     (type:string 'VideotexString)]
  [(VisibleString)      (type:string 'VisibleString)]
  ;; UnrestrictedCharacterStringType
  [(CHARACTER STRING)   (type:string 'Character-String)])

(define-nt UsefulType
  [(GeneralizedTime)    (type 'GeneralizedTime)]
  [(UTCTime)            (type 'UTCTime)]
  [(ObjectDescriptor)   (type 'ObjectDescriptor)])

;; ----------------------------------------
;; 12 Constructed types, tagging, etc (pdf 233)

(define-nt TaggedType
  [(Tag Type)           (type:tagged $1 #f        $2)]
  [(Tag IMPLICIT Type)  (type:tagged $1 'implicit $3)]
  [(Tag EXPLICIT Type)  (type:tagged $1 'explicit $3)])

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

(define-nt SelectionType
  [(Identifier LESSTHAN Type) (type:select $1 $3)])

;; ============================================================
;; VALUES:

(define-nt BuiltinValue
  [(NULL) (value 'NULL)]
  [(TRUE) (value #t)]
  [(FALSE) (value #f)]
  [(num) (value $1)]
  [(bstring) (value:bstring $1)] ;; BIT STRING, OCTET STRING
  [(hstring) (value:hstring $1)] ;; BIT STRING, OCTET STRING
  [(cstring) (value $1)] ;; character string types
  [(Identifier COLON Value) (value:choice $1 $3)]
  ;; [(LBRACE Identifier* RBRACE) (value:bit-list $2)] ;; BIT STRING; overlaps SE[QT]-OF
  [(LBRACE Value* RBRACE) (value:seq/set-of $2)]
  [(LBRACE NamedValue* RBRACE) (value:seq/set $2)]
  [(LBRACE GenOIDComponents+ RBRACE) (value:oid/reloid $2)] ;; = ObjectIdentifierValue
  ;; ----------------------------------------
  [(ObjectIdentifierValue) $1]
  #;[(EmbeddedPDVValue) $1]
  #;[(ExternalValue) $1]
  [(Type COLON Value)  ;; spec has [(ObjectClassFieldValue) $1], overlaps with BuiltinValue
   (value:annotated $1 $3)]
  #;[(RealValue) $1]
  #;[(TaggedValue) $1])

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

(define-nt ConstrainedType
  [(Type Constraint)    (type:constrained $1 $2)]
  [(TypeWithConstraint) $1])

(define-nt TypeWithConstraint
  ;;[(SEQUENCE Constraint OF Type) ...]
  ;;[(SET Constraint OF Type) ...]
  [(SEQUENCE SizeConstraint OF Type)
   (type:sequence-of $4 $2)]
  [(SET SizeConstraint OF Type)
   (type:set-of $4 $2)])

(define-nt SingleValue
  [(Value) (constraint:single-value $1)])

(define-nt ContainedSubtype
  [(INCLUDES Type) (constraint:includes $2)]
  [(Type) (constraint:includes $1)])

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

(define-nt ObjectClass
  [(DefinedObjectClass) $1]
  [(ObjectClassDefn) $1]
  [(ParameterizedObjectClass) $1])

(define-nt ObjectClassDefn
  [(CLASS LBRACE FieldSpec+ RBRACE WithSyntaxSpec) (class:defn $3 $5)])

(define-nt+ FieldSpec+ FieldSpec #:sep [COMMA])

(define-nt FieldSpec
  [(TypeFieldSpec) $1]
  [(FixedTypeValueFieldSpec) $1]
  [(VariableTypeValueFieldSpec) $1]
  [(FixedTypeValueSetFieldSpec) $1]
  [(VariableTypeValueSetFieldSpec) $1]
  [(ObjectFieldSpec) $1]
  [(ObjectSetFieldSpec) $1])

(define-nt TypeFieldSpec
  [(TypeFieldReference TypeOptionalitySpec) (field:type $1 $2)])

(define-nt TypeOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Type) (opt:default #f $2)] [() #f])

(define-nt FixedTypeValueFieldSpec
  [(ValueFieldReference Type Unique ValueOptionalitySpec)
   (field:value/fixed-type $1 $2 $3 $4)])

(define-nt Unique
  [(UNIQUE) 'unique] [() #f])

(define-nt ValueOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Value) (opt:default #f $2)] [() #f])

(define-nt VariableTypeValueFieldSpec
  [(ValueFieldReference Type-FieldName ValueOptionalitySpec)
   (field:value/var-type $1 $2 $3)])

(define-nt FixedTypeValueSetFieldSpec
  [(ValueSetFieldReference Type ValueSetOptionalitySpec)
   (field:value-set/fixed-type $1 $2 $3)])

(define-nt ValueSetOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT ValueSet) (opt:default #f $2)] [() #f])

(define-nt VariableTypeValueSetFieldSpec
  [(ValueSetFieldReference Type-FieldName ValueSetOptionalitySpec)
   (field:value-set/var-type $1 $2 $3)])

(define-nt ObjectFieldSpec
  [(ObjectFieldReference DefinedObjectClass ObjectOptionalitySpec)
   (field:object $1 $2 $3)])

(define-nt ObjectOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT Object) (opt:default #f $2)] [() #f])

(define-nt ObjectSetFieldSpec
  [(ObjectSetFieldReference DefinedObjectClass ObjectSetOptionalitySpec)
   (field:object-set $1 $2 $3)])

(define-nt ObjectSetOptionalitySpec
  [(OPTIONAL) (opt:optional #f)] [(DEFAULT ObjectSet) (opt:default #f $2)] [() #f])

(begin ;; DEPRECATED: imprecise
  (define-nt FieldName
    [(PrimitiveFieldName+) $1])
  (define-nt+ PrimitiveFieldName+ PrimitiveFieldName #:sep [DOT])
  (define-nt PrimitiveFieldName
    [(TypeFieldReference) $1]
    [(ValueFieldReference) $1]
    [(ValueSetFieldReference) $1]
    [(ObjectFieldReference) $1]
    [(ObjectSetFieldReference) $1]))

(begin ;; BETTER, not quite precise
  (define-nt* Object/Set-Field* Object/Set-Field #:post [DOT])
  (define-nt Object/Set-Field
    [(ObjectFieldReference) $1]
    [(ObjectSetFieldReference) $1])
  (define-nt Type-FieldName
    [(Object/Set-Field* TypeFieldReference) (cons $1 $2)])
  (define-nt Value-FieldName
    [(Object/Set-Field* ValueFieldReference) (cons $1 $2)])
  (define-nt ValueSet-FieldName
    [(Object/Set-Field* ValueSetFieldReference) (cons $1 $2)])
  (define-nt Object-FieldName
    [(Object/Set-Field* ObjectFieldReference) (cons $1 $2)])
  (define-nt ObjectSet-FieldName
    [(Object/Set-Field* ObjectSetFieldReference) (cons $1 $2)]))

(define-nt Object
  [(ObjectDefn) $1]
  [(DefinedObject) $1]
  [(ObjectFromObject) $1]
  [(ParameterizedObject) $1])

(define-nt ObjectDefn
  ;; DefaultSyntax
  [(LBRACE FieldSetting* RBRACE) (object:defn $2)]
  ;; DefinedSyntax
  [(LBRACE DefinedSyntaxToken* RBRACE) (object:sugar $2)])

(define-nt*+ FieldSetting* FieldSetting+ FieldSetting #:sep [COMMA])

(define-nt FieldSetting
  [(TypeFieldReference Type) (ast:named $1 $2)]
  [(ValueFieldReference Value) (ast:named $1 $2)]
  [(ValueSetFieldReference ValueSet) (ast:named $1 $2)]
  [(ObjectFieldReference Object) (ast:named $1 $2)]
  [(ObjectSetFieldReference ObjectSet) (ast:named $1 $2)])

;; IMPRECISE
;; (define-nt FieldSetting
;;   [(PrimitiveFieldName Setting) (fixme $1 $2)])

(define-nt Setting
  [(Type) $1]
  [(Value) $1]
  [(ValueSet) $1]
  [(Object) $1]
  [(ObjectSet) $1])

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
  [(WORD) (sugar:literal $1)]
  [(ReservedWORD) (sugar:literal $1)]
  [(COMMA) (sugar:literal #\,)])

(define-nt* DefinedSyntaxToken* DefinedSyntaxToken #:post [])

(define-nt DefinedSyntaxToken
  [(Setting) $1]
  [(Literal) $1])

;; 15.5 Value sets and information object sets (pdf 357)

(define-nt ObjectSet
  [(LBRACE ObjectSetSpec RBRACE)
   (object-set:defn $2)])

(define-nt ObjectSetSpec
  [(RootElementSetSpec OptionalExtensionMarker) $1]
  [(ELLIPSIS) null])

(define-nt ValueSet
  [(LBRACE ElementSetSpecs RBRACE)
   (value-set:defn $2)])

(define-nt ObjectSetElements
  [(Object) $1]
  [(DefinedObjectSet) $1]
  [(ObjectSetFromObjects) $1]
  [(ParameterizedObjectSet) $1])

;; 15.6 Accessing the information stored in objects and object sets (pdf 364)

;; FIXME: simplify/unify ASTs?

(define-nt ValueFromObject
  [(ReferencedObjects DOT Value-FieldName)
   (value:from-object $1 $3)])

(define-nt ValueSetFromObjects
  [(ReferencedObjects DOT ValueSet-FieldName)
   (value-set:from-object $1 $3)])

(define-nt TypeFromObject
  [(ReferencedObjects DOT Type-FieldName)
   (type:from-object $1 $3)])

(define-nt ObjectFromObject
  [(ReferencedObjects DOT Object-FieldName)
   (object:from-object $1 $3)])

(define-nt ObjectSetFromObjects
  [(ReferencedObjects DOT ObjectSet-FieldName)
   (object-set:from-object $1 $3)])

(define-nt ReferencedObjects
  [(DefinedObject) $1]
  [(DefinedObjectSet) $1]
  [(ParameterizedObject) $1]
  [(ParameterizedObjectSet) $1])

;; 15.7 (pdf 369)

(define-nt ObjectClassFieldType
  [(DefinedObjectClass DOT FieldName)
   (type:from-class $1 $3)])

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

(define-nt UsefulObjectClassReference
  [(TYPE-IDENTIFIER) (class:type-identifier)]
  #;[(ABSTRACT-SYNTAX) ?])

(define-nt InstanceOfType
  [(INSTANCE OF DefinedObjectClass) (type:instance-of $3)])

;; TYPE-IDENTIFIER ::= CLASS { &id OBJECT IDENTIFIER UNIQUE, &Type }
;; WITH SYNTAX { &Type IDENTIFIED BY id } -- ??? id, not &id ???

;; 15.10 ABSTRACT-SYNTAX (skip)

;; ============================================================
;; 16 Macros (skip)

;; ============================================================
;; 17 Paramerization (pdf 405)

(define-nt ParameterizedAssignment
  [(ParameterizedTypeAssignment) $1]
  [(ParameterizedValueAssignment) $1]
  [(ParameterizedValueSetAssignment) $1]
  [(ParameterizedObjectClassAssignment) $1]
  [(ParameterizedObjectAssignment) $1]
  [(ParameterizedObjectSetAssignment) $1])

(define-nt ParameterizedTypeAssignment
  [(Word ParameterList ASSIGN Type)
   (assign:type $1 $2 $4)])

(define-nt ParameterizedValueAssignment
  [(Identifier ParameterList Type ASSIGN Value)
   (assign:value $1 $2 $3 $5)])

(define-nt ParameterizedValueSetAssignment
  [(Word ParameterList Type ASSIGN ValueSet)
   (assign:value-set $1 $2 $3 $5)])

(define-nt ParameterizedObjectClassAssignment
  [(WORD ParameterList ASSIGN ObjectClass)
   (assign:class $1 $2 $4)])

(define-nt ParameterizedObjectAssignment
  [(Identifier ParameterList DefinedObjectClass ASSIGN Object)
   (assign:object $1 $2 $3 $5)])

(define-nt ParameterizedObjectSetAssignment
  [(Word ParameterList DefinedObjectClass ASSIGN ObjectSet)
   (assign:object-set $1 $2 $3 $5)])

(define-nt ParameterList
  [(LBRACE Parameter+ RBRACE) $2])

(define-nt+ Parameter+ Parameter #:sep [COMMA])

(define-nt Parameter
  [(ParamGovernor COLON DummyReference) (param $1 $3)]
  [(DummyReference) (param #f $1)])
(define-nt ParamGovernor
  [(Governor) $1]
  [(DummyGovernor) $1])
(define-nt Governor
  [(Type) $1]
  [(DefinedObjectClass) $1])

(define-nt DummyGovernor
  [(DummyReference) $1])

(define-nt DummyReference
  [(Reference) $1])

(define-nt ParameterizedType
  [(SimpleDefinedType ActualParameterList)
   (expr:apply $1 $2)])
;; ParameterizedValueSetType ambiguous w/ ParameterizedType

(define-nt SimpleDefinedType
  [(ExternalTypeReference) $1]
  [(TypeReference) $1])

(define-nt ParameterizedValue
  [(SimpleDefinedValue ActualParameterList)
   (expr:apply $1 $2)])

(define-nt SimpleDefinedValue
  [(ExternalValueReference) $1]
  [(ValueReference) $1])

(define-nt ParameterizedObjectClass
  [(DefinedObjectClass ActualParameterList)
   (expr:apply $1 $2)])

(define-nt ParameterizedObject
  [(DefinedObject ActualParameterList)
   (expr:apply $1 $2)])

(define-nt ParameterizedObjectSet
  [(DefinedObjectSet ActualParameterList)
   (expr:apply $1 $2)])

(define-nt ActualParameterList
  [(LBRACE ActualParameter+ RBRACE) $2])

(define-nt+ ActualParameter+ ActualParameter #:sep [COMMA])

(define-nt ActualParameter
  [(Type) $1]
  [(Value) $1]
  [(ValueSet) $1]
  [(DefinedObjectClass) $1]
  [(Object) $1]
  [(ObjectSet) $1])

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
             #:end (id word word-caps END)))

;; ============================================================

(module+ main
  (require racket/pretty
           racket/list
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
          (send asn1-module-header-parser parse* (asn1-lexer in)))
         (let loop ()
           (define rs (send asn1-assignment-parser parse* (asn1-lexer in)))
           (printf "\n-- ~s --\n" (length rs))
           (pretty-print (remove-duplicates rs))
           (unless (for/and ([r (in-list rs)]) (eq? (token-value r) #f))
             (loop))))))))
