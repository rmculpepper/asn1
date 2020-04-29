#lang racket/base
(require racket/match
         racket/list
         "ast.rkt")

;; Need pre-passes
;; - desugar information objects
;; - mark dependent SET/SEQUENCE fields
;; - resolve tagging mode to implicit/explicit
;; - topological sort definitions by dependence?
;; - disambiguate NULL type vs NULL value (eg in object)

(define current-env (make-parameter (hash)))

(define (lookup-class ref)
  (define env (current-env))
  (let loop ([ref ref])
    (match (hash-ref env ref #f)
      [(assign:class _ _ (? class:defn? defn)) defn]
      [_ #f])))

(define (desugar-object sugar classref)
  (match (lookup-class classref)
    [(class:defn cs pattern)
     (desugar sugar pattern)]
    [v #f]))

(define (desugar sugar pattern)
  (let loop ([sugar sugar] [pattern pattern] [acc null])
    (eprintf "desugar:\n ~v\n ~v\n ~v\n\n" sugar pattern acc)
    (match* [sugar pattern]
      [[(cons lit sugar) (cons lit pattern)] ;; nonlinear!
       (loop sugar pattern acc)]
      [[sugar (cons (sugar:optional group) pattern)]
       (or (loop sugar (append group pattern) acc)
           (loop sugar pattern acc))]
      [[(cons value sugar) (cons (? field-ref? ref) pattern)]
       (loop sugar pattern (cons (ast:named (id-of ref) value) acc))]
      [['() '()]
       (reverse acc)]
      [[_ _] #f])))

(define (field-ref? x)
  (or (ref:type-field? x)
      (ref:value-field? x)
      (ref:value-set-field? x)
      (ref:object-field? x)
      (ref:object-set-field? x)))


;; ----------------------------------------

(define (decl-of-module m)
  (match m
    [(mod:defn id tagmode extmode exports imports assignments)
     ;; FIXME: id, tagmode, extmode, exports, imports
     (define env (env-of-assignments assignments))
     (parameterize ((current-env env))
       (decl-of-assignments assignments))]))

(define (env-of-assignments assignments)
  (for/fold ([env #hash()]) ([assignment (in-list assignments)])
    (match assignment
      [(assign:class name params class) (hash-set env name assignment)]
      [_ env])))

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
      `(define ,(header-of name params) ,(expr-of type))
      (decls-for-type type))]
    [(assign:value name params type value)
     `(define ,(header-of name params) ,(expr-of value #:kind type))]
    [(assign:value-set name params type value-set)
     `(define ,(header-of name params) ,(expr-of value-set #:kind type))]
    [(assign:class name params class)
     `(define ,(header-of name params) ,(expr-of class))]
    [(assign:object name params class object)
     `(define ,(header-of name params) ,(expr-of object #:kind class))]
    [(assign:object-set name params class object-set)
     `(define ,(header-of name params) ,(expr-of object-set #:kind class))]))

(define (id-of ref)
  (match ref
    [(ref:value name) name]
    [(ref:type name) name]
    [(ref:module name) name]
    [(ref:class name) name]
    [(ref:object name) name]
    [(ref:object-set name) name]
    [(ref:type-field name) name]
    [(ref:value-field name) name]
    [(ref:value-set-field name) name]
    [(ref:object-field name) name]
    [(ref:object-set-field name) name]))

(define (formal-of param)
  '(FIXME-PARAM))

(define (expr-of x #:kind [kind #f])
  (match x

    ;; ----------------------------------------
    ;; TYPE
    [(ref:type name) name]
    [(type name) name]
    [(type:bit-string _) 'BIT-STRING]
    [(type:choice alts) `(CHOICE ,@(map choice-alt-of alts))]
    [(type:integer _) 'INTEGER]
    [(type:sequence fields) `(SEQUENCE ,@(map seq/set-field-of fields))]
    [(type:set fields) `(SET ,@(map seq/set-field-of fields))]
    [(type:set-of type size-c) `(SET-OF ,(expr-of type))] ;; FIXME: size-c
    [(type:sequence-of type size-c) `(SEQUENCE-OF ,(expr-of type))] ;; FIXME: size-c
    [(type:string subtype) subtype]
    [(type:constrained type constraint) (expr-of type)] ;; FIXME
    [(type:any-defined-by id) 'ANY] ;; FIXME
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
           ,(expr-of type))]
    #;[(type:from-object object field) '(FIXME)]
    #;[(type:instance-of oid) '(FIXME)]
    #;[(type:select id type) '(FIXME)]
    #;[(expr:apply type args) '(FIXME)]
    [(ref:type name) name]

    ;; ----------------------------------------
    ;; VALUE
    [(ref:value name) name]
    [(? exact-nonnegative-integer? n) n]
    ['NULL 'NULL] ;; Ambiguous type vs value! NULL type probably more common. (FIXME?)
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
     `(list (quote ,name) ,(expr-of value))] ;; FIXME: alt type
    [(value:seq/set-of values)
     ;; FIXME: elem type
     `(list ,@(for/list ([value (in-list values)]) (expr-of value)))]
    [(value:seq/set values)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              values)))]
    ;; [(value:from-object object field)
    ;;  '(FIXME)]
    [(value:annotated type value)
     (expr-of value #:type type)]

    ;; ----------------------------------------
    ;; VALUE SET
    [(value-set:defn vs)
     (let loop ([vs vs])
       (match vs
         [(constraint:or vs1 vs2)
          (make-append (loop vs1) (loop vs2))]
         [(constraint:single-value v)
          `(list ,(expr-of v #:kind type))]
         [(ref:object-set name) name] ;; grammar oddity ???
         [_
          `(FIXME (Value-Set ,vs))]))]

    ;; ----------------------------------------
    ;; CLASS

    ;; ----------------------------------------
    ;; OBJECT
    [(ref:object name) name]
    [(object:defn decls)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              decls)))]
    [(object:sugar sugar)
     (cond [(desugar-object sugar kind)
            => (lambda (decls)
                 (expr-of (object:defn decls)))]
           [else `(FIXME (Sugar ,sugar ,kind ,(lookup-class kind)))])]

    ;; ----------------------------------------
    ;; OBJECT SET
    [(ref:object-set name) name]
    [(object-set:defn objs)
     (let loop ([objs objs])
       (match objs
         [(constraint:or objs1 objs2)
          (make-append (loop objs1) (loop objs2))]
         [(constraint:single-value obj)
          `(list ,(expr-of obj #:kind kind))]
         [(ref:object-set name) name]
         [(ref:object name) `(list ,name)]
         [(? object:defn? obj) `(list ,(expr-of obj #:kind kind))]
         [(? object:sugar? obj) `(list ,(expr-of obj #:kind kind))]
         [_
          `(FIXME (Objects ,objs))]))]

    ;; ----------------------------------------
    ;; GENERIC
    [(expr:apply thing args)
     `(,(expr-of thing) ,@(map expr-of args))]

    ;; ----------------------------------------
    [_ `(FIXME '(expr ,x))]))


(define (decls-for-type t)
  (define (make-def x)
    (match-define (ast:named name value) x)
    `(define ,name ,(expr-of value #:kind t)))
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
     `[,name ,(expr-of type) #:optional]]
    [(opt:default (ast:named name type) default)
     `[,name ,(expr-of type) #:default ,(expr-of default #:kind type)]]
    [(ast:named name type)
     `[,name ,(expr-of type)]]))

(define (choice-alt-of a)
  (match a
    [(ast:named name type)
     `[,name ,(expr-of type)]]
    [_ `(FIXME (Alt ,a))]))

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
