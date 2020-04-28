#lang racket/base
(require racket/match
         racket/list
         "ast.rkt")

;; Need pre-passes
;; - desugar information objects
;; - mark dependent SET/SEQUENCE fields
;; - resolve tagging mode to implicit/explicit
;; - topological sort definitions by dependence?

(define (decl-of-module m)
  (match m
    [(mod:defn id tagmode extmode exports imports assignments)
     ;; FIXME: id, tagmode, extmode, exports, imports
     (decl-of-assignments assignments)]))

(define (decl-of-assignments assignments)
  (cons 'begin (map decl-of-assignment assignments)))

(define (decl-of-assignment a)
  (define (header-of name params)
    (cond [(null? params) (id-of name)]
          [else (cons (id-of name) (map formal-of params))]))
  (define (do-begin a bs) (if (null? bs) a `(begin ,a ,@bs)))
  (match a
    [(assign:type name params type)
     (do-begin
      `(define ,(header-of name params) ,(expr-of-type type))
      (decls-for-type type))]
    [(assign:value name params type value)
     `(define ,(header-of name params) ,(expr-of-value value #:type type))]
    [(assign:value-set name params type value-set)
     `(define ,(header-of name params) ,(expr-of-value-set value-set #:type type))]
    [(assign:class name params class)
     `(define ,(header-of name params) ,(expr-of-class class))]
    [(assign:object name params class object)
     `(define ,(header-of name params) ,(expr-of-object object #:class class))]
    [(assign:object-set name params class object-set)
     `(define ,(header-of name params) ,(expr-of-object-set object-set #:class class))]))

(define (id-of ref)
  (match ref
    [(ref:value name) name]
    [(ref:type name) name]
    [(ref:module name) name]
    [(ref:class name) name]
    [(ref:object name) name]
    [(ref:object-set name) name]))

(define (formal-of param)
  '(FIXME-PARAM))

(define (expr-of-type t)
  (match t
    [(type name) name]
    [(type:bit-string _) 'BIT-STRING]
    [(type:choice alts) `(CHOICE ,@(map choice-alt-of alts))]
    [(type:integer _) 'INTEGER]
    [(type:sequence fields) `(SEQUENCE ,@(map seq/set-field-of fields))]
    [(type:set fields) `(SET ,@(map seq/set-field-of fields))]
    [(type:set-of type size-c) `(SET-OF ,(expr-of-type type))] ;; FIXME: size-c
    [(type:sequence-of type size-c) `(SEQUENCE-OF ,(expr-of-type type))] ;; FIXME: size-c
    [(type:string subtype) subtype]
    [(type:constrained type constraint) (expr-of-type type)] ;; FIXME
    [(type:any-defined-by id) 'ANY] ;; FIXME
    ;; ----
    [(type:enum _) 'ENUMERATED]
    [(type:tagged (tag tagclass tagnum) mode type)
     `(TAG ,(match tagclass
              ['universal '#:universal]
              ['application '#:application]
              ['context-sensitive '#:context-sensitive]
              ['private '#:private])
           ,@(match mode
               ['implicit '(#:implicit)]
               ['explicit '(#:explicit)]
               [#f '(#:explicit)]) ;; FIXME
           ,tagnum
           ,(expr-of-type type))]
    #;[(type:from-object object field) '(FIXME)]
    #;[(type:instance-of oid) '(FIXME)]
    #;[(type:select id type) '(FIXME)]
    #;[(expr:apply type args) '(FIXME)]
    [(ref:type name) name]
    [_ `(FIXME '(Type ,t))]))

(define (decls-for-type t)
  (define (make-def x)
    (match-define (ast:named name value) x)
    `(define ,name ,(expr-of-value value #:type t)))
  (match t
    [(type:bit-string (? list? names))
     (map make-def names)]
    [(type:integer (? list? names))
     (map make-def names)]
    [(type:enum (? list? names))
     (map make-def names)]
    [_ null]))

(define (seq/set-field-of f)
  (match f
    [(opt:optional (ast:named name type))
     `[,name ,(expr-of-type type) #:optional]]
    [(opt:default (ast:named name type) default)
     `[,name ,(expr-of-type type) #:default ,(expr-of-value default #:type type)]]
    [(ast:named name type)
     `[,name ,(expr-of-type type)]]))

(define (choice-alt-of a)
  (match a
    [(ast:named name type)
     `[,name ,(expr-of-type type)]]
    [_ `(FIXME (Alt ,a))]))

(define (expr-of-value value #:type [type #f])
  (match value
    [(? exact-nonnegative-integer? n) n]
    ['NULL #f]
    ['TRUE #t]
    ['FALSE #f]
    [(value:oid/reloid cs)
     (define (const-oid? cs) (andmap const-oid-component? cs))
     (match cs
       [(cons (ref:value base-oid) (? const-oid? cs))
        `(build-OID base-oid ,@(map sexpr-of-oid-component cs))]
       [(cons (ref:value base-oid) cs)
        `(append base-oid (list ,@(map expr-of-oid-component cs)))]
       [(? const-oid? cs)
        `(OID ,@(map sexpr-of-oid-component cs))]
       [cs
        `(list ,@(map expr-of-oid-component cs))])]
    [(value:oid/reloid components)
     `(OID ,@(map sexpr-of-oid-component components))]
    [(value:choice name value)
     ;; FIXME: alt type
     `(list (quote ,name) ,(expr-of-value value #:type #f))] ;; FIXME: alt type
    [(value:seq/set-of values)
     ;; FIXME: elem type
     `(list ,@(for/list ([value (in-list values)]) (expr-of-value value #:type #f)))]
    [(value:seq/set values)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of-value value #:type #f))])
                              values)))]
    ;; [(value:from-object object field)
    ;;  '(FIXME)]
    [(value:annotated type value)
     (expr-of-value value #:type type)]
    [_ `(FIXME (Value ,value))]))

(define (const-oid-component? c)
  (or (exact-nonnegative-integer? c) (ast:named? c)))

(define (sexpr-of-oid-component c)
  (match c
    [(? exact-nonnegative-integer? n) n]
    [(ast:named name num) `(,name ,num)]))

(define (expr-of-oid-component c)
  (match c
    [(ref:value name) name]
    [(? exact-nonnegative-integer? n) n]
    [(ast:named name num) num]))

(define (expr-of-value-set vs #:type [type #f])
  (let loop ([vs vs])
    (match vs
      [(constraint:or vs1 vs2)
       (make-append (loop vs1) (loop vs2))]
      [(constraint:single-value v)
       `(list ,(expr-of-value v #:type type))]
      [_
       `(FIXME (Value-Set ,vs))])))

(define (expr-of-class class)
  '(FIXME))

(define (expr-of-object obj #:class [class #f])
  `(FIXME (Object ,obj)))

(define (expr-of-object-set object-set #:class [class #f])
  (match object-set
    [(object-set:defn objs)
     (let loop ([objs objs])
       (match objs
         [(constraint:or objs1 objs2)
          (make-append (loop objs1) (loop objs2))]
         [(constraint:single-value obj)
          `(list ,(expr-of-object obj #:class class))]
         [_
          `(FIXME (Objects ,objs))]))]
    [_ `(FIXME (Object-Set ,object-set))]))

(define (make-append xse yse)
  (match* [xse yse]
    [[`(list ,@xs) `(list ,@ys)]
     `(list ,@xs ,@ys)]
    [[`(append ,@xsse) yse]
     `(append ,@xsse ,yse)]
    [[xse yse] `(append ,xse ,yse)]))

;; ============================================================

(module+ main
  (require parser-tools/lex
           racket/pretty
           racket/cmdline
           "grammar.rkt"
           "lexer.rkt")
  (define the-parser asn1-module-parser)
  (define the-translate decl-of-module)
  (command-line
   #:once-any
   [("-a") "Parse an assignment list"
    (set! the-parser asn1-assignments-parser)
    (set! the-translate decl-of-assignments)]
   [("-m") "Parse a single module (default)"
    (void)]
   #:args files
   (for ([file files])
     (call-with-input-file* file
       (lambda (in)
         (port-count-lines! in)
         (pretty-print
          (the-translate
           (the-parser (lambda () (get-token in))))))))))
