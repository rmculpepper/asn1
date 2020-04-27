#lang racket/base
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

;; Tokens (Dubuisson 8.3.2, p100 (pdf 128))

(define get-token
  (lexer-src-pos
   ;; whitespace
   [(:or #\space #\tab #\newline #\return)
    (return-without-pos (get-token input-port))]
   ;; comment ::= [-][-] .* (NEWLINE | [-][-])
   [(:seq "--"
          (:* (:seq (:? #\-) (:~ (:or #\newline #\return #\-))))
          (:or #\newline #\return "--"))
    (return-without-pos (get-token input-port))]
   ;; ----------------------------------------
   ;; bstring ::= ['] [01]* ['][B], maybe also whitespace (discarded)
   [(:seq #\' (:or #\0 #\1) #\' #\B)
    (token-bstring lexeme)]
   ;; cstring ::= ["](""|[^"])*["]
   [(:seq #\" (:* (:or (:seq #\" #\") (:~ #\"))) #\")
    (token-cstring lexeme)]
   ;; hstring ::= ['] [0-9A-F]* ['][H]
   [(:seq #\' (:* (:/ #\0 #\9 #\A #\F)) #\' #\H)
    (token-hstring lexeme)]
   ;; identifier ::= [a-z][-A-Z0-9]* -- but no double-dash, last char must not be dash
   ;;     ::= [a-z] ([-]?[a-zA-Z0-9])*
   [(:seq (:/ #\a #\z) (:* (:? #\-) (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-id lexeme)]
   ;; number ::= 0 | [1-9][0-9]*
   [(:+ (:/ #\0 #\9))
    (token-num (string->number lexeme))]
   ;; signed number
   [(:seq (:? #\-) (:+ (:/ #\0 #\9)))
    (token-num (string->number lexeme))]
   ;; typereference ::= [A-Z]([-]?[a-zA-Z0-9])*, not reserved word
   ;; word ::= same as typereference
   [(:seq (:/ #\A #\Z) (:* (:? #\-) (:/ #\a #\z #\A #\Z #\0 #\9)))
    (or (hash-ref reserved-word-h lexeme #f)
        (token-word lexeme))]
   ;; ----------------------------------------
   [(:seq #\& (:/ #\a #\z) (:* (:? #\-) (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-amp-id (substring lexeme 1))]
   [(:seq #\& (:/ #\A #\Z) (:* (:? #\-) (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-amp-word (substring lexeme 1))]
   ;; modulereference = typereference
   ;; objectclassreference ::= typereference, but lower-case chars not allowed
   ;; objectfieldreference ::= [&] objectreference
   ;; objectreference ::= valuereference
   ;; objectsetfieldreference ::= [&] objectsetreference
   ;; objectsetreference ::= typereference
   ;; typefieldreference ::= [&]typefieldreference
   ;; valuefieldreference ::= [&]valuereference
   ;; valuereference ::= identifier
   ;; valuesetfieldreference ::= [&]typereference
   ;; ----------------------------------------

   ["::=" 'ASSIGN]
   ["(" 'LPAREN]
   [")" 'RPAREN]
   ["{" 'LBRACE]
   ["}" 'RBRACE]
   ["[" 'LBRACKET]
   ["]" 'RBRACKET]
   ["[[" 'LLBRACKET]
   ["]]" 'RRBRACKET]
   ["," 'COMMA]
   [":" 'COLON]
   [";" 'SEMICOLON]
   ["." 'DOT]
   [".." 'DOTDOT]
   ["..." 'ELLIPSIS]
   ["@" 'AT]
   ["|" 'PIPE]

   ;; ----------------------------------------
   [(eof)
    'EOF]))

(define reserved-words
  '(ABSENT ABSTRACT-SYNTAX ALL APPLICATION AUTOMATIC BEGIN BIT BMPString
    BOOLEAN BY CHARACTER CHOICE CLASS COMPONENT COMPONENTS CONSTRAINED
    CONTAINING DEFAULT DEFINITIONS EMBEDDED ENCODED END ENUMERATED EXCEPT
    EXPLICIT EXPORTS EXTENSIBILITY EXTERNAL FALSE FROM GeneralizedTime
    GeneralString GraphicString IA5String IDENTIFIER IMPLICIT IMPLIED
    IMPORTS INCLUDES INSTANCE INTEGER INTERSECTION ISO646String MAX MIN
    MINUS-INFINITY NULL NumericString OBJECT ObjectDescriptor OCTET OF
    OPTIONAL PATTERN PDV PLUS-INFINITY PRESENT PrintableString PRIVATE
    REAL RELATIVE-OID SEQUENCE SET SIZE STRING SYNTAX T61String TAGS
    TeletexString TRUE TYPE-IDENTIFIER UNION UNIQUE UNIVERSAL
    UniversalString UTCTime UTF8String VideotexString VisibleString WITH

    ANY DEFINED))

(define reserved-word-h
  (for/hash ([w (in-list reserved-words)]) (values (symbol->string w) w)))

;; ------------------------------------------------------------
(require (only-in "parser-util.rkt" [define-tokens define-tokens+]))

(define-tokens+ asn1-tokens
  #:tokens
  [bstring
   cstring
   hstring
   id
   num
   word
   amp-id
   amp-word
   ]

  #:empty-tokens
  [ABSENT ABSTRACT-SYNTAX ALL APPLICATION AUTOMATIC BEGIN BIT BMPString
   BOOLEAN BY CHARACTER CHOICE CLASS COMPONENT COMPONENTS CONSTRAINED
   CONTAINING DEFAULT DEFINITIONS EMBEDDED ENCODED END ENUMERATED EXCEPT
   EXPLICIT EXPORTS EXTENSIBILITY EXTERNAL FALSE FROM GeneralizedTime
   GeneralString GraphicString IA5String IDENTIFIER IMPLICIT IMPLIED
   IMPORTS INCLUDES INSTANCE INTEGER INTERSECTION ISO646String MAX MIN
   MINUS-INFINITY NULL NumericString OBJECT ObjectDescriptor OCTET OF
   OPTIONAL PATTERN PDV PLUS-INFINITY PRESENT PrintableString PRIVATE
   REAL RELATIVE-OID SEQUENCE SET SIZE STRING SYNTAX T61String TAGS
   TeletexString TRUE TYPE-IDENTIFIER UNION UNIQUE UNIVERSAL
   UniversalString UTCTime UTF8String VideotexString VisibleString WITH

   ANY DEFINED

   ASSIGN
   LPAREN
   RPAREN
   LBRACE
   RBRACE
   LBRACKET
   RBRACKET
   LLBRACKET
   RRBRACKET
   COMMA
   COLON
   SEMICOLON
   DOT
   DOTDOT
   ELLIPSIS
   AT
   PIPE

   EOF])


;; ------------------------------------------------------------

(module+ main
  (require racket/file)
  (for ([file (current-command-line-arguments)])
    (define in (open-input-file file))
    (let loop ()
      (define next (get-token in))
      (printf "~e\n" next)
      (unless (eq? (token-name (position-token-token next)) 'EOF)
        (loop)))))
