#lang racket/base
(require "lexer.rkt")

;; ============================================================

(define-syntax-rule (define-nt* NT* Elem)
  (define-nt NT*
    [() null]
    [([x Elem] [xs NT*]) (cons x xs)]))

(define-syntax-rule (define-nt*+ NT* NT+ Elem [Sep ...])
  (begin
    (define-nt NT*
      [() null]
      [([xs NT+]) xs])
    (define-+-nt NT+ Elem Sep ...)))

(define-syntax-rule (define-nt+ NT+ Elem [Sep ...])
  (define-nt NT+
    [([x Elem]) (list x)]
    [([x Elem] Sep ... [xs NT+]) (cons x xs)]))

;; no info object, classes, etc

;; no extension alternative (only extensibility marker)
;; - delete SET, SEQUENCE, CHOICE ExtensionAdditionAlternatives

;; ============================================================
;; 9.1 Assignments (p 106, pdf 134)

(define-nt Assignment
  [(TypeReference ASSIGN Type)
   (ast:type-assignment $1 $3)]
  [(ValueReference Type ASSIGN Value)
   (ast:value-assignment $1 $2 $4)]
  [(TypeReference Type ASSIGN ValueSet)
   _]
  [(ObjectClassReference ASSIGN ObjectClass)
   _]
  [(ObjectReference DefinedObjectClass ASSIGN Object)
   _]
  [(ObjectSetReference DefinedObjectClass ASSIGN ObjectSet)
   _])

(define-nt Type
  [(BuiltinType) $1]
  [(ReferencedType) $1]
  [(ConstrainedType) $1])

(define-nt BuiltinValue
  [(BitStringValue) $1]
  [(BooleanValue) $1]
  [(CharacterStringValue) $1]
  [(ChoiceValue) $1]
  [(EmbeddedPDVValue) $1]
  [(EnumeratedValue) $1]
  [(ExternalValue) $1]
  [(InstanceOfValue) $1]
  [(IntegerValue) $1]
  [(NullValue) $1]
  [(ObjectClassFieldValue) $1]
  [(ObjectIdentifierValue) $1]
  [(OctetStringValue) $1]
  [(RealValue) $1]
  [(RelativeOIDValue) $1]
  [(SequenceOfValue) $1]
  [(SequenceValue) $1]
  [(SetOfValue) $1]
  [(SetValue) $1]
  [(TaggedValue) $1])

(define-nt ReferencedValue
  [(DefinedValue) $1]
  [(ValueFromObject) $1])

(define-nt TaggedValue
  [(Value) $1])

;; ============================================================
;; 9.2 Module Structure (pdf 141)

(define-nt ModuleDefinition
  [(ModuleIdentifier DEFINITIONS TagDefault ExtensionDefault
                     ASSIGN BEGIN ModuleBody END)
   (fixme $1 $3 $4 $7)])

(define-nt ModuleIdentifier
  [(ModuleReference DefinitiveIdentifier)
   (fixme $1 $2)])

(define-nt DefinitiveIdentifier
  [(LBRACE DefinitiveObjectIdComponents+ RBRACE)
   (fixme $2)]
  [() #f])

(define-nt+ DefinitiveObjectIdComponents+ DefinitiveObjectIdComponent #:sep [])

(define-nt DefinitiveObjectIdComponent
  [(Identifier) $1]
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (fixme $1 $3)])

(define-nt ExtensionDefault
  [(EXTENSIBILITY IMPLIED) #t]
  [() #f])

(define-nt ModuleBody
  [(Exports Imports AssignmentList)
   (fixme $1 $2 $3)]
  [()
   (list null null null)])

(define-nt Exports
  [(EXPORTS SEMICOLON) null]
  [(EXPORTS SymbolsExported+ SEMICOLON) $2]
  [(EXPORTS ALL SEMICOLON) #f]
  [() #f])

(define-nt+ SymbolsExported+ Symbol #:sep [COMMA])

(define-nt Imports
  [(IMPORTS SymbolsImported SEMICOLON)
   $2]
  [() #f])

(define-nt* SymbolsImported SymbolsFromModule)

(define-nt SymbolsFromModule
  [(Symbols+ FROM GlobalModuleReference) (fixme $1 $3)])

(define-nt+ Symbols+ Symbol #:sep [])

(define-nt GlobalModuleReference
  [(ModuleReference AssignedIdentifier) (fixme $1 $2)])

(define-nt AssignedIdentifier
  [(ObjectIdentifierValue) $1]
  [(DefinedValue) $1]
  [() #f])

(define-nt Symbol
  [(Reference) $1]
  [(ParameterizedReference) $1])

(define-nt Reference
  [(TypeReference) $1]
  [(ObjectClassReference) $1]
  [(ObjectSetReference) $1])

(define-nt ParameterizedReference
  [(Reference) $1]
  [(Reference LBRACE RBRACE) $1])

;; ============================================================
;; 9.3 Local and external references (pdf 146)

(define-nt DefinedType
  [(ExternalTypeReference) $1]
  [(TypeReference) $1]
  [(ParameterizedType) $1]
  [(ParameterizedValueSetType) $1])

(define-nt ExternalTypeReference
  [(ModuleReference DOT TypeReference) (fixme $1 $3)])

(define-nt DefinedValue
  [(ExternalValueReference) $1]
  [(ValueReference) $1]
  [(ParameterizedValue) $1])

(define-nt ExternalValueReference
  [(ModuleReference DOT ValueReference) (fixme $1 $3)])

(define-nt DefinedObjectClass
  [(ExternalObjectClassReference) $1]
  [(ObjectClassReference) $1]
  [(UsefulObjectClassReference) $1])

(define-nt ExternalObjectClassReference
  [(ModuleReference DOT ObjectClassReference) (fixme $1 $3)])

(define-nt DefinedObject
  [(ExternalObjectReference) $1]
  [(ObjectReference) $1])

(define-nt ExternalObjectReference
  [(ModuleReference DOT ObjectReference) (fixme $1 $3)])

(define-nt DefinedObjectSet
  [(ExternalObjectSetReference) $1]
  [(ObjectSetReference) $1])

(define-nt ExternalObjectSetReference
  [(ModuleReference DOT ObjectSetReference) (fixme $1 $3)])

;; ============================================================
;; 10 Basic Types (pdf 155)

(define-nt BooleanType
  [(BOOLEAN) $1])

(define-nt BooleanValue
  [(TRUE) 'true]
  [(FALSE) 'false])

(define-nt NullType
  [(NULL) $1])

(define-nt NullValue
  [(NULL) $1])

(define-nt IntegerType
  [(INTEGER) (type:integer #f)]
  [(INTEGER LBRACE NamedNumber+ RBRACE) (type:integer $3)])

(define-nt+ NamedNumber+ NamedNumber #:sep [COMMA])

(define-nt NamedNumber
  [(Identifier LPAREN SignedNumber RPAREN) (fixme $1 $3)]
  [(Identifier LPAREN DefinedValue RPAREN) (fixme $1 $3)])

(define-nt IntegerValue
  [(SignedNumber) $1]
  [(Identifier) $1])

(define-nt EnumeratedType
  [(ENUMERATED LBRACE Enumerations+ RBRACE)
   (type:enum $3)])

(define-nt Enumerations+
  [(Enumeration) $1]
  [(Enumeration COMMA ELLIPSIS ExceptionSpec)
   ;; FIXME
   $1]
  [(Enumeration COMMA ELLIPSIS ExceptionSpec COMMA Enumeration+)
   ;; FIXME
   $1])

(define-nt+ Enumeration EnumerationItem #:sep [COMMA])

(define-nt EnumerationItem
  [(Identifier) $1]
  [(NamedNumber) $1])

(define-nt EnumeratedValue
  [(Identifier) $1])

;; FIXME: REAL

(define-nt BitStringType
  [(BIT STRING)
   (type:bit-string null)]
  [(BIT STRING LBRACE NamedBit+ RBRACE)
   (type:bit-string $4)])

(define-nt+ NamedBit+ NamedBit #:sep [COMMA])

(define-nt NamedBit
  [(Identifier LPAREN Number RPAREN)
   (fixme $1 $3)]
  [(Identifier LPAREN DefinedValue RPAREN)
   (fixme $1 $3)])

(define-nt BitStringValue
  [(BString) $1]
  [(HString) $1]
  [(IdentifierList) $1])

(define-nt IdentifierList
  [(LBRACE RBRACE) null]
  [(LBRACE Identifier+ RBRACE) $2])

(define-nt+ Identifier+ Identifier #:sep [COMMA])

(define-nt OctetStringType
  [(OCTET STRING) (type:octet-string)])

(define-nt OctetStringValue
  [(BString) $1]
  [(HString) $1])

(define-nt ObjectIdentifierType
  [(OBJECT IDENTIFIER) (type:object-identifier)])

(define-nt ObjectIdentifierValue
  [(LBRACE ObjIdComponents+ RBRACE) $2]
  [(LBRACE DefinedVAlue ObjIdComponents RBRACE) (cons $2 $3)])

(define-nt+ ObjIdComponents+ ObjIdComponent #:sep [])

(define-nt ObjIdComponent
  [(Identifier) $1]
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (fixme $1 $3)]
  [(DefinedValue) (fixme $1)]) ;; defined as RELATIVE OID or INTEGER

(define-nt RelativeOIDType
  [(RELATIVE-OID) (type:relative-oid)])

(define-nt RelativeOIDValue
  [(LBRACE RelativeOIDComponents+ RBRACE) $2])

(define-nt+ RelativeOIDComponents+ RelativeOIDComponent #:sep [])

(define-nt RelativeOIDComponent
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (fixme $1 $3)]
  [(DefinedValue) (fixme $1)]) ;; defined as RELATIVE OID

;; Strings are complicated, simplify ...

(define-nt CharacterStringType
  [(RestrictedCharacterStringType) $1]
  [(UnrestrictedcharacterStringType) $1])

(define-nt RestrictedCharacterStringType
  [(BMPString)       (type:string 'BMPString)]
  [(GeneralString)   (type:string 'GeneralString)]
  [(GraphicString)   (type:string 'GraphicString)]
  [(IA5String)       (type:string 'IA5String)]
  [(ISO646String)    (type:string 'ISO656String)]
  [(NumericString)   (type:string 'NumericString)]
  [(PrintableString) (type:string 'PrintableString)]
  [(TeletexString)   (type:string 'TeletexString)]
  [(T61String)       (type:string 'T61String)]
  [(UniversalString) (type:string 'UniversalString)]
  [(UTF8String)      (type:string 'UTF8String)]
  [(VideotexString)  (type:string 'VideotexString)]
  [(VisibleString)   (type:string 'VisibleString)])

(define-nt UnrestrictedCharacterStringType
  [(CHARACTER STRING) (type:string 'Character-String)])

(define-nt CharacterStringValue
  [(CString) $1])
;; FIXME

(define-nt UsefulType
  [(GeneralizedTime) (type 'GeneralizedTime)]
  [(UTCTime) (type 'UTCTime)]
  [(ObjectDescriptor) (type 'ObjectDescriptor)])

;; ============================================================
;; 12 Constructed types... (pdf 233)

(define-nt TaggedType
  [(Tag Type)          (type:tagged $1 'auto     $2)]
  [(Tag IMPLICIT Type) (type:tagged $1 'implicit $2)]
  [(Tag EXPLICIT Type) (type:tagged $1 'explicit $2)])

(define-nt Tag
  [(LBRACKET Class ClassNumber RBRACKET) (fixme $2 $4)])

(define-nt Class
  [(UNIVERSAL)   'universal]
  [(APPLICATION) 'application]
  [(PRIVATE)     'private]
  [()            'context-sensitive])

(define-nt ClassNumber
  [(Number) $1]
  [(DefinedValue) $1])

(define-nt TagDefault
  [(EXPLICIT TAGS) 'explicit]
  [(IMPLICIT TAGS) 'implicit]
  [(AUTOMATIC TAGS) 'automatic]
  [() 'explicit])

(define-nt SequenceType
  [(SEQUENCE LBRACE RBRACE)
   (type:sequence null)]
  [(SEQUENCE LBRACE ComponentTypeLists RBRACE)
   (type:sequence $3)])

(define-nt ComponentTypeLists
  [(RootComponentTypeList)
   $1]
  [(RootComponentTypeList OptionalExtensionMarker)
   $1])

(define-nt RootComponentTypeList
  [(ComponentTypeList+) _])

(define-nt+ ComponentTypeList+ ComponentType #:sep [COMMA])

(define-nt ComponentType
  [(NamedType) $1]
  [(NamedType OPTIONAL) (fixme 'optional $1)]
  [(NamedType DEFAULT Value) (fixme 'default $1 $3)]
  [(COMPONENTS OF Type) (fixme 'components-of $3)])

(define-nt NamedType
  [(Identifier Type) (fixme $1 $2)])

(define-nt SequenceValue
  [(LBRACE NamedValue* RBRACE) (value:sequence $2)])

(define-nt*+ NamedValue* NamedValue+ NamedValue #:sep [COMMA])

(define-nt NamedValue
  [(Identifier Value) (fixme $1 $2)])

(define-nt SetType
  [(SET LBRACE RBRACE)
   (type:set null)]
  [(SET LBRACE ComponentTypeLists RBRACE)
   (type:set $3)])

(define-nt SetValue
  [(LBRACE NamedValue* RBRACE) (value:set $2)])

(define-nt SequenceOfType
  [(SEQUENCE OF Type) (type:sequence-of $3)])

;; FIXME TypeWithConstraint

(define-nt SizeConstraint
  [(SIZE Constraint) (fixme $2)])

(define-nt SequenceOfValue
  [(LBRACE Value* RBRACE) (value:sequence-of $2)])

(define-nt*+ Value* Value+ Value #:sep [COMMA])

(define-nt SetOfType
  [(SET OF Type) (type:set-of $3)])

(define-nt SetOfValue
  [(LBRACE Value* RBRACE) (value:set-of $2)])

(define-nt ChoiceType
  [(CHOICE LBRACE AlternativeTypeLists RBRACE)
   (type:choise $3)])

(define-nt AlternativeTypeLists
  [(RootAlternativeTypeList)
   $1]
  [(RootAlternativeTypeList OptionalExtensionMarker)
   $1])

(define-nt RootAlternativeTypeList
  [(AlternativeTypeList) $1])

(define-nt AlternativeTypeList
  [(NamedType+) $1])

(define-nt+ NamedType+ NamedType #:sep [COMMA])

(define-nt ChoiceValue
  [(Identifier COLON Value) (value:choice $1 $3)])

(define-nt SelectionType
  [(Identifier LESSTHAN Type) (fixme $1 $3)])

(define-nt OptionalExtensionMarker
  [(COMMA ELLIPSIS) #t]
  [() #f])

;; ============================================================
;; 13 Subtype constraints (pdf 285)

(define-nt ConstrainedType
  [(Type Constraint) (type:constrained $1 $2)])
;; FIXME

(define-nt Constraint
  [(LPAREN ConstraintSpec RPAREN) (fixme $2 $3)])

(define-nt ConstraintSpec
  [(ElementSetSpecs) $1]
  [(GeneralConstraint) $1])

(define-nt ElementSetSpec
  [(Unions) $1])
(define-nt Unions
  [(Intersections) $1]
  [(Unions UnionMark Intersections) (fixme 'union $1 $3)])
(define-nt UnionMark
  [(UNION) #t]
  [(PIPE) #t])

(define-nt Intersections
  [(IntersectionElements) $1]
  [(Intersections INTERSECTION IntersectionElements) (fixme 'intersection $1 $3)])
(define-nt IntersectionElements
  [(Elements) $1])
(define-nt Elements
  [(SubtypeElements) $1])

(define-nt SubtypeElements
  [(Value)
   (constraint:single-value $1)]
  [(Value DOTDOT Value) ;; FIXME
   (constraint:interval $1 $3)]
  [(SIZE Constraint)
   (constraint:size $2)])
;; FROM
;; WITH COMPONENT, WITH COMPONENTS
;; CONTAINING, ENCODED BY
;; UNION, INTERSECTION, EXCEPT
;; CONSTRAINED BY

;; ============================================================
;; 17 Paramerized things... (pdf 412)

(define-nt ParameterizeAssignment
  [(ParameterizedTypeAssignment) $1]
  [(ParameterizedValueAssignment) $1])

(define-nt ParameterizedTypeAssignment
  [(TypeReference ParameterList ASSIGN Type)
   _])

(define-nt ParameterizedValueAssignment
  [(ValueReference ParameterList Type ASSIGN Value)
   _])

(define-nt ParameterList
  [(LBRACE Parameter+ RBRACE) $2])
(define-nt+ Parameter+ Parameter #:sep [COMMA])

(define-nt Parameter
  [(ParamGovernor COLON DummyReference) (fixme $1 $3)]
  [(DummyReference) (fixme $1)])
(define-nt ParamGovernor
  [(Type) $1]
  [(DummyReference) $1])
(define-nt DummyReference
  [(Reference) $1])
(define-nt Reference
  [(TypeReference) $1]
  [(ValueReference) $1]) ;; FIXME: omits object, etc

(define-nt ParameterizedType
  [(SimpleDefinedType ActualParameterList)
   (fixme $1 $2)])
(define-nt SimpleDefinedType
  [(ExternalTypeReference) $1]
  [(TypeReference) $1])

(define-nt ParameterizedValue
  [(SimpleDefinedValue ActualParameterList)
   (fixme $1 $2)])
(define-nt SimpleDefinedValue
  [(ExternalValueReference) $1]
  [(ValueReference) $1])

(define-nt ActualParameterList
  [(LBRACE ActualParameter+ RBRACE) $2])
(define-nt+ ActualParameter+ ActualParameter #:sep [COMMA])

(define-nt ActualParameter
  [(Type) $1]
  [(Value) $1])
