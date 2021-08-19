#lang racket/base
(require racket/match
         racket/string
         grrparse
         grrparse/lex)
(provide (all-defined-out))

;; Tokens (Dubuisson 8.3.2, p100 (pdf 128))

;; SRX =
;; | pregexp
;; | (srx:* SRX)
;; | (srx:seq (Listof SRX))
;; | (srx:cs String)

(struct srx:alt (es) #:prefab)
(struct srx:seq (es) #:prefab)
(struct srx:postfix (e op) #:prefab)
(struct srx:cs (cs) #:prefab)

(define (:or . es) (srx:alt es))
(define (:seq . es) (srx:seq es))
(define (:* e) (srx:postfix e '*))
(define (:+ e) (srx:postfix e '+))
(define (:? e) (srx:postfix e '?))
(define (:cs cs) (srx:cs cs))

(define (px srx)
  (define (loop/0 srx)
    (match srx
      [(? pregexp? px)
       (object-name px)]
      [(? string? s)
       (regexp-quote s)]
      [(srx:alt es)
       (string-join (map loop/0 es) "|")]
      [_ (loop/1 srx)]))
  (define (loop/1 srx)
    (match srx
      [(srx:seq es)
       (string-join (map loop/1 es) "")]
      [_ (loop/2 srx)]))
  (define (loop/2 srx)
    (match srx
      [(srx:postfix e op) (format "~a~a" (loop/2 e) op)]
      [(srx:cs cs) (format "[~a]" cs)]
      [(or (? pregexp?) (? string?)
           (? srx:alt?) (? srx:seq?) (? srx:postfix?) (? srx:cs?))
       (format "(?:~a)" (loop/0 srx))]
      [_ (error 'px "bad: ~e" srx)]))
  (pregexp (loop/0 srx)))

(define ((K v) lm s e) v)

;; ----------------------------------------

(define asn1-token-reader
  (regexps-token-reader
   ;; whitespace
   #px"\\s+"
   (K #f)

   ;; comment ::= [-][-] .* (NEWLINE | [-][-])
   #;(px (:seq "--"
               (:* (:seq (:? #\-) (:cs "^\\-\n\r")))
               (:or (:cs "\n\r") "--")))
   #px"--(?:-?[^-\n\r])*(?:\n|\r|--)"
   (K #f)

   ;; ----------------------------------------

   ;; bstring ::= ['] [01]* ['][B], maybe also whitespace (discarded)
   #px"'[01]*'B"
   (lambda (lm s e) (token 'bstring (substring lm 1 (- (string-length lm) 2))))

   ;; cstring ::= ["](""|[^"])*["]
   (px (:seq "\"" (:* (:or "\"\"" (:cs "^\""))) "\""))
   (lambda (lm s e)
     (define sub (substring lm 1 (- (string-length lm) 1)))
     (token 'cstring (regexp-replace* #rx"\"\"" sub "\"")))

   ;; hstring ::= ['] [0-9A-F]* ['][H]
   #px"'[0-9A-F]*'H"
   (lambda (lm s e) (token 'hstring (substring lm 1 (- (string-length lm) 2))))

   ;; identifier ::= [a-z][-A-Z0-9]* -- but no double-dash, last char must not be dash
   ;;     ::= [a-z] ([-]?[a-zA-Z0-9])*
   ;; typereference ::= [A-Z]([-]?[a-zA-Z0-9])*, not reserved word
   ;; word ::= same as typereference
   #px"[a-zA-Z](?:-?[a-zA-Z0-9])*"
   (lambda (lm s e)
     (cond [(hash-ref reserved-word-h lm #f)
            => values]
           [(regexp-match? #rx"^[a-z]" lm)
            (token 'id (string->symbol lm))]
           [(regexp-match? #rx"[a-z]" lm)
            (token 'word (string->symbol lm))]
           [else (token 'word-caps (string->symbol lm))]))

   ;; number ::= 0 | [1-9][0-9]*
   #px"[0-9]+"
   (lambda (lm s e) (token 'num (string->number lm)))

   ;; signed number
   ;;   [(:seq (:? #\-) (:+ (:/ #\0 #\9)))
   ;;    (token-num (string->number lexeme))]

   ;; ----------------------------------------

   #px"[&a-zA-Z](?:-?[a-zA-Z0-9])*"
   (lambda (lm s e)
     (cond [(regexp-match? #rx"^&[a-z]" lm)
            (token 'amp-id (string->symbol lm))]
           [else (token 'amp-word (string->symbol lm))]))

   ;; ----------------------------------------

   (px "::=") (K 'ASSIGN)
   (px "(")   (K 'LPAREN)
   (px ")")   (K 'RPAREN)
   (px "{")   (K 'LBRACE)
   (px "}")   (K 'RBRACE)
   (px "[")   (K 'LBRACKET)
   (px "]")   (K 'RBRACKET)
   (px "[[")  (K 'LLBRACKET)
   (px "]]")  (K 'RRBRACKET)
   (px ",")   (K 'COMMA)
   (px ":")   (K 'COLON)
   (px ";")   (K 'SEMICOLON)
   (px ".")   (K 'DOT)
   (px "..")  (K 'DOTDOT)
   (px "...") (K 'ELLIPSIS)
   (px "@")   (K 'AT)
   (px "|")   (K 'PIPE)
   (px "<")   (K 'LESSTHAN)
   (px "^")   (K 'CARET)))

(define reserved-words
  '(ABSENT ALL APPLICATION AUTOMATIC BEGIN BIT BOOLEAN BY
    CHARACTER CHOICE CLASS COMPONENT COMPONENTS CONSTRAINED CONTAINING 
    DEFAULT DEFINITIONS EMBEDDED ENCODED END ENUMERATED EXCEPT
    EXPLICIT EXPORTS EXTENSIBILITY EXTERNAL FALSE FROM
    IDENTIFIER IMPLICIT IMPLIED IMPORTS INCLUDES INSTANCE INTEGER INTERSECTION
    MAX MIN MINUS-INFINITY NULL OBJECT OCTET OF OPTIONAL
    PATTERN PDV PLUS-INFINITY PRESENT PRIVATE
    REAL RELATIVE-OID SEQUENCE SET SIZE STRING SYNTAX
    TAGS TRUE UNION UNIQUE UNIVERSAL WITH

    ANY DEFINED))

;; ASN.1 considers these reserved, but just treat them as defined names.
(define removed-reserved-words
  '(BMPString GeneralizedTime GeneralString GraphicString IA5String ISO646String
    NumericString ObjectDescriptor PrintableString T61String TeletexString
    UniversalString UTCTime UTF8String VideotexString VisibleString
    ABSTRACT-SYNTAX TYPE-IDENTIFIER))

(define reserved-word-h
  (for/hash ([w (in-list reserved-words)]) (values (symbol->string w) w)))

(define asn1-lexer (make-lexer asn1-token-reader))

;; ------------------------------------------------------------

(module+ main
  (require racket/file)
  (for ([file (current-command-line-arguments)])
    (define tz (asn1-lexer (open-input-file file)))
    (let loop ()
      (define next ((tokenizer-get-token tz) 'default null))
      (printf "~e\n" next)
      (unless (eq? (token-name next) 'EOF)
        (loop)))))
