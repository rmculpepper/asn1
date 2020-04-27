#lang racket/base
(require "parser-util.rkt"
         "lexer.rkt"
         "ast.rkt")

(use-tokens! asn1-tokens)

(define-nt Identifier [(id) $1])
(define-nt ModuleReference [(word) $1])
(define-nt TypeReference [(word) $1])
(define-nt ValueReference [(id) $1])
(define-nt Number [(num) $1])

;; ============================================================

(define-syntax-rule (define-nt* NT* Elem)
  (define-nt NT*
    [() null]
    [([x Elem] [xs NT*]) (cons x xs)]))

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
;; Types

(define-nt Type
  [(BuiltinType) $1]
  [(ReferencedType) $1]
  [(ConstrainedType) $1])

(define-nt ReferencedType
  [(DefinedType) $1]
  [(UsefulType) $1]
  #;[(SelectionType) $1]
  #;[(TypeFromObject) $1]
  #;[(ValueSetFromObjects) $1])

(define-nt BuiltinType
  [(BIT STRING LBRACE NamedBit+ RBRACE) (type:bit-string $4)]
  [(BIT STRING) (type:bit-string null)]
  [(BOOLEAN) (type 'boolean)]
  [(CHOICE LBRACE AlternativeTypeLists RBRACE) (type:choice $3)]
  [(ENUMERATED LBRACE Enumerations RBRACE) (type:enum $3)]
  [(INTEGER LBRACE NamedNumber+ RBRACE) (type:integer $3)]
  [(INTEGER) (type:integer #f)]
  [(NULL) (type 'null)]
  [(OBJECT IDENTIFIER) (type 'oid)]
  [(OCTET STRING) (type 'octet-string)]
  [(RELATIVE-OID) (type 'relative-oid)]
  [(SEQUENCE LBRACE ComponentTypeLists RBRACE) (type:sequence $3)]
  [(SEQUENCE OF Type) (type:sequence-of $3 #f)]
  [(SET LBRACE ComponentTypeLists RBRACE) (type:set $3)]
  [(SET OF Type) (type:set-of $3 #f)]
  ;; ----
  ;;[(EmbeddedPDVType) $1]
  ;;[(ExternalType) $1]
  ;;[(ObjectClassFieldType) $1]
  ;;[(InstanceOfType) $1]
  ;;[(RealType) $1]
  [(CharacterStringType) $1]
  [(TaggedType) $1])

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

(define-nt TaggedType
  [(Tag Type)           (type:tagged $1 #f        $2)]
  [(Tag IMPLICIT Type)  (type:tagged $1 'implicit $3)]
  [(Tag EXPLICIT Type)  (type:tagged $1 'explicit $3)])

(define-nt UsefulType
  [(GeneralizedTime)    (type 'GeneralizedTime)]
  [(UTCTime)            (type 'UTCTime)]
  [(ObjectDescriptor)   (type 'ObjectDescriptor)])

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

;; ----------------------------------------
;; Aux nonterminals for types

;; INTEGER
(begin
  (define-nt+ NamedNumber+ NamedNumber #:sep [COMMA])
  (define-nt NamedNumber
    [(Identifier LPAREN Number RPAREN) (fixme $1 $3)]
    [(Identifier LPAREN DefinedValue RPAREN) (fixme $1 $3)]))

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
     (fixme $1 $3)]
    [(Identifier LPAREN DefinedValue RPAREN)
     (fixme $1 $3)]))

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
    [(NamedType OPTIONAL) (fixme 'optional $1)]
    [(NamedType DEFAULT Value) (fixme 'default $1 $3)]
    [(COMPONENTS OF Type) (fixme 'components-of $3)])
  (define-nt NamedType
    [(Identifier Type) (fixme $1 $2)]))

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
    [(LBRACKET Class ClassNumber RBRACKET) (fixme $2 $3)])
  (define-nt Class
    [(UNIVERSAL)   'universal]
    [(APPLICATION) 'application]
    [(PRIVATE)     'private]
    [()            'context-sensitive])
  (define-nt ClassNumber
    [(Number) $1]
    [(DefinedValue) $1]))

;; constrained types
(begin
  (define-nt SizeConstraint
    [(SIZE Constraint) $2]))

;; (define-nt SelectionType
;;   [(Identifier LESSTHAN Type) (fixme $1 $3)])


;; ============================================================
;; Values

(define-nt Value
  [(BuiltinValue) $1]
  [(ReferencedValue) $1])

(define-nt ReferencedValue
  [(DefinedValue) $1]
  #;[(ValueFromObject) $1])

(define-nt BuiltinValue
  [(NULL) 'NULL]
  [(TRUE) '#t]
  [(FALSE) '#f]
  [(num) $1]
  [(bstring) $1] ;; BIT STRING, OCTET STRING
  [(hstring) $1] ;; BIT STRING, OCTET STRING
  [(cstring) $1] ;; character string types
  [(IdentifierList) $1] ;; --- bit string value
  [(Identifier COLON Value) (value:choice $1 $3)]
  [(LBRACE Value* RBRACE) (value:seq/set-of $2)]
  [(LBRACE NamedValue* RBRACE) (value:seq/set $2)]
  ;; ----------------------------------------
  [(ObjectIdentifierValue) $1]
  #;[(EmbeddedPDVValue) $1]
  #;[(ExternalValue) $1]
  #;[(InstanceOfValue) $1]
  #;[(ObjectClassFieldValue) $1]
  #;[(RealValue) $1]
  #;[(TaggedValue) $1])

(define-nt IdentifierList
  [(LBRACE RBRACE) null]
  [(LBRACE Identifier+ RBRACE) $2])

(define-nt+ Identifier+ Identifier #:sep [COMMA])

(define-nt ObjectIdentifierValue
  [(LBRACE GenOIDComponents+ RBRACE) (value:oid/reloid $2)])

(define-nt+ GenOIDComponents+ GenOIDComponent #:sep [])

(define-nt GenOIDComponent
  [(Number) $1]
  [(Identifier LPAREN Number RPAREN) (fixme $1 $3)]
  ;; DefinedValue contains Identifier: OID/Rel-OID or INTEGER
  [(DefinedValue) $1])

(define-nt*+ Value* Value+ Value #:sep [COMMA])

(define-nt*+ NamedValue* NamedValue+ NamedValue #:sep [COMMA])

(define-nt NamedValue
  [(Identifier Value) (named-value $1 $2)])

;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


;; ============================================================
;; 9.1 Assignments (p 106, pdf 134)

(define-nt* AssignmentList Assignment)

(define-nt Assignment
  [(TypeReference ASSIGN Type)
   (assign:type $1 $3)]
  [(ValueReference Type ASSIGN Value)
   (assign:value $1 $2 $4)]
  #;[(TypeReference Type ASSIGN ValueSet) _]
  #;[(ObjectClassReference ASSIGN ObjectClass) _]
  #;[(ObjectReference DefinedObjectClass ASSIGN Object) _]
  #;[(ObjectSetReference DefinedObjectClass ASSIGN ObjectSet) _])


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
  [(ValueReference) $1]
  #;[(ObjectClassReference) $1]
  #;[(ObjectReference) $1]
  #;[(ObjectSetReference) $1])

(define-nt ParameterizedReference
  [(Reference) $1]
  [(Reference LBRACE RBRACE) $1])

;; ============================================================
;; 9.3 Local and external references (pdf 146)

(define-nt DefinedType
  [(ExternalTypeReference) $1]
  [(TypeReference) $1]
  [(ParameterizedType) $1]
  #;[(ParameterizedValueSetType) $1])

(define-nt ExternalTypeReference
  [(ModuleReference DOT TypeReference) (fixme $1 $3)])

(define-nt DefinedValue
  [(ExternalValueReference) $1]
  [(ValueReference) $1]
  [(ParameterizedValue) $1])

(define-nt ExternalValueReference
  [(ModuleReference DOT ValueReference) (fixme $1 $3)])

;; (define-nt DefinedObjectClass
;;   [(ExternalObjectClassReference) $1]
;;   [(ObjectClassReference) $1]
;;   [(UsefulObjectClassReference) $1])
;; (define-nt ExternalObjectClassReference
;;   [(ModuleReference DOT ObjectClassReference) (fixme $1 $3)])
;; (define-nt DefinedObject
;;   [(ExternalObjectReference) $1]
;;   [(ObjectReference) $1])
;; (define-nt ExternalObjectReference
;;   [(ModuleReference DOT ObjectReference) (fixme $1 $3)])
;; (define-nt DefinedObjectSet
;;   [(ExternalObjectSetReference) $1]
;;   [(ObjectSetReference) $1])
;; (define-nt ExternalObjectSetReference
;;   [(ModuleReference DOT ObjectSetReference) (fixme $1 $3)])


;; ============================================================
;; 13 Subtype constraints (pdf 285)

(define-nt Constraint
  [(LPAREN ConstraintSpec RPAREN) $2])

(define-nt ConstraintSpec
  [(ElementSetSpecs) $1]
  #;[(GeneralConstraint) $1])

(define-nt ElementSetSpecs
  [(Unions) $1])

(define-nt Unions
  [(Intersections) $1]
  [(Unions UnionMark Intersections) (constraint:or $1 $3)])

(define-nt UnionMark
  [(UNION) #t]
  [(PIPE) #t])

(define-nt Intersections
  [(IntersectionElements) $1]
  [(Intersections INTERSECTION IntersectionElements) (constraint:and $1 $3)])

(define-nt IntersectionElements
  [(Elements) $1])

(define-nt Elements
  [(SubtypeElements) $1])

(define-nt SubtypeElements
  [(Value)
   (constraint:value $1)]
  [(LoValue DOTDOT HiValue) ;; FIXME
   (constraint:interval $1 $3)]
  [(SIZE Constraint)
   (constraint:size $2)])

(define-nt LoValue
  [(Value) $1]
  [(MIN) 'MIN])
(define-nt HiValue
  [(Value) $1]
  [(MAX) 'MAX])

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
   (assign:type-fun $1 $2 $4)])

(define-nt ParameterizedValueAssignment
  [(ValueReference ParameterList Type ASSIGN Value)
   (assign:value-fun $1 $2 $3 $5)])

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


;; ============================================================
(require (prefix-in yacc: parser-tools/cfg-parser))

(define asn1-parser
  (parser #:parser-form yacc:cfg-parser
          #:start ModuleDefinition
          #:end EOF
          #:error (lambda args (error 'asn1-parser "failed: ~e" args))))

;; ============================================================

(module+ main
  (for/list ([file (current-command-line-arguments)])
    (call-with-input-file* file
      (lambda (in) (printf "~v\n" (asn1-parser (lambda () (get-token in))))))))
