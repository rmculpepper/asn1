;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         "ast.rkt"
         "tree-util.rkt")

;; Other ideas
;; - topological sort definitions by dependence?

;; Pre-pass
;; - mark dependent SET/SEQUENCE fields

;; ============================================================
;; Translation

(define show-type? (make-parameter #f))
(define current-fixme-mode (make-parameter 'comment))
(define split-definitions? (make-parameter #t))

(define (decl-of-module m)
  (match m
    [(mod:defn (mod:id id _) tagmode extmode exports imports assignments)
     (define defs (unbegin (decl-of-assignments assignments)))
     `(module ,id racket/base
        (require asn1 asn1/util/names)
        (provide (all-defined-out))
        ,@(map decl-of-import imports)
        ,@(cond [(split-definitions?)
                 (define-values (tdefs vdefs)
                   (partition (match-lambda [`(define-asn1-type . ,_) #t] [_ #f])
                              defs))
                 `[,(unquoted-printing-string ";; Value, etc definitions")
                   ,@(mark-duplicates vdefs)
                   ,(unquoted-printing-string ";; Type definitions")
                   ,@tdefs
                   ,@(if (null? tdefs) (list (unquoted-printing-string "")) null)]]
                [else (mark-duplicates defs)]))]))

(define (decl-of-import imp)
  (match-define (mod:import syms (mod:id name _)) imp)
  `(require (only-in (submod ".." ,name) ,@syms)))

(define (decl-of-assignments assignments)
  (cons 'begin (apply append (map unbegin (map decl-of-assignment assignments)))))

(define (decl-of-assignment a)
  (define (do-begin a bs) (if (null? bs) a `(begin ,a ,@bs)))
  (match a
    [(assign:type name type)
     `(begin
        (define-asn1-type ,name ,(expr-of type))
        ,@(decls-for-type type name))]
    [(assign:value name type value)
     `(define ,name
        ,@(if (show-type?) (comments (format "~s" (expr-of type))) null)
        ,(expr-of value))]
    [(assign:value-set name type value-set)
     `(define ,name
        ,@(if (show-type?) (comments (format "~s" (expr-of type))) null)
        ,(expr-of value-set))]
    [(assign:class name class)
     `(define ,name ,(expr-of class))]
    [(assign:object name class object)
     `(define ,name
        ,@(if (show-type?) (comments (format "~s" (expr-of class))) null)
        ,(expr-of object #:class class))]
    [(assign:object-set name class object-set)
     `(define ,name
        ,@(if (show-type?) (comments (format "~s" (expr-of class))) null)
        ,(expr-of object-set #:class class))]))

(define (formal-of p)
  (match p [(param gov ref) ref]))

(define (expr-of x #:class [class #f])
  (match x

    ;; ----------------------------------------
    ;; TYPE
    #;[(ref:type name) name]
    [(type name) name]
    [(type:bit-string '()) 'BIT-STRING]
    [(type:bit-string nvs)
     `(WRAP-NAMES BIT-STRING
                  (list ,@(map (match-lambda
                                 [(ast:named name value)
                                  `(cons ',name ,(expr-of value))])
                               nvs)))]
    [(type:choice alts) `(CHOICE ,@(map choice-alt-of alts))]
    [(type:enum nvs)
     `(WRAP-NAMES ENUMERATED
                  (list ,@(map (match-lambda
                                 [(ast:named name value)
                                  `(cons ',name ,(expr-of value))])
                               nvs)))]
    [(type:integer _) 'INTEGER]
    [(type:sequence fields) `(SEQUENCE ,@(map seq/set-field-of fields))]
    [(type:set fields) `(SET ,@(map seq/set-field-of fields))]
    [(type:sequence-of type size-c)
     (begin/fixme (and size-c `(Constraint ,(expr-of size-c)))
                  `(SEQUENCE-OF ,(expr-of type)))]
    [(type:set-of type size-c)
     (begin/fixme (and size-c `(Constraint ,(expr-of size-c)))
                  `(SET-OF ,(expr-of type)))]
    [(type:string subtype) subtype]
    [(type:tagged (tag tagclass tagnum) mode type)
     `(TAG ,@(match mode
               ['implicit '(#:implicit)]
               ['explicit '(#:explicit)]
               [#f '(#:explicit)])
           ,@(match tagclass
               ['universal '(#:universal)]
               ['application '(#:application)]
               ['context-sensitive '()]
               ['private '(#:private)])
           ,tagnum
           ,(expr-of type))]
    ;; ----
    [(type:constrained (type:from-class class fields) (constraint:table objset ats))
     `(object-set-ref ,(expr-of class) ,(expr-of objset) (quote ,fields)
                      ,@(if (pair? ats) (comments "(with @-constraints)") null))]
    [(type:constrained type constraint)
     (begin/fixme `(Constraint ,(expr-of constraint)) (expr-of type))]
    [(type:any-defined-by id)
     `(begin ANY ,@(comments (format "DEFINED BY ~s" id)))]
    [(type:from-object object field) (do-field-ref object field)]
    [(type:from-class class field)
     `(class-ref ,(expr-of class) ',field)]
    [(type:instance-of oid) `(FIXME '(instance-of ,(expr-of oid)))]
    [(type:select id type) `(FIXME '(select ,id ,(expr-of type)))]

    ;; ----------------------------------------
    ;; VALUE
    #;[(ref:value name) name]
    [(value v) v]
    [(value:bstring s) `(FIXME '(bstring ,s))]
    [(value:hstring s) `(FIXME '(hstring ,s))]
    [(value:annotated type value) (expr-of value)]
    [(value:bit-list bits) `(FIXME '(bits (list ,@bits)))]
    [(value:choice name value)
     `(list (quote ,name) ,(expr-of value))]
    [(value:oid/reloid cs)
     (define (const-oid? cs) (andmap const-oid-component? cs))
     (match cs
       [(cons (? id? base-oid) (? const-oid? cs))
        `(build-OID ,base-oid ,@(map sexpr-of-oid-component cs))]
       [(cons (? id? base-oid) cs)
        `(append ,base-oid (list ,@(map expr-of-oid-component cs)))]
       [(? const-oid? cs)
        `(OID ,@(map sexpr-of-oid-component cs))]
       [cs
        `(list ,@(map expr-of-oid-component cs))])]
    [(value:seq/set-of vals)
     `(list ,@(for/list ([val (in-list vals)]) (expr-of val)))]
    [(value:seq/set vals)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              vals)))]
    [(value:from-object object field) (do-field-ref object field)]

    ;; ----------------------------------------
    ;; VALUE SET
    [(value-set:defn vs)
     (let loop ([vs vs])
       (match vs
         [(constraint:or vs1 vs2)
          (make-append (loop vs1) (loop vs2))]
         [(constraint:single-value v)
          `(list ,(expr-of v #:kind type))]
         [(? id? name) `(list ,name)]
         [(? word? name) name]
         [_ `(FIXME '(Value-Set ,vs))]))]
    [(value-set:from-object object field) (do-field-ref object field)]

    ;; ----------------------------------------
    ;; CLASS
    #;[(ref:class name) name]
    [(class:defn fields _)
     `(ObjectClass ,@(map sexpr-of-class-field fields))]
    [(class:primitive name) name]

    ;; ----------------------------------------
    ;; OBJECT
    #;[(ref:object name) name]
    [(object:defn decls)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              decls)))]
    [(object:from-object object field) (do-field-ref object field)]

    ;; ----------------------------------------
    ;; OBJECT SET
    #;[(ref:object-set name) name]
    [(object-set:defn objs)
     (let loop ([objs objs])
       (match objs
         [(constraint:or objs1 objs2)
          (make-append (loop objs1) (loop objs2))]
         [(constraint:single-value obj)
          `(list ,(expr-of obj #:class class))]
         [(object-set:one obj)
          `(list ,(expr-of obj #:class class))]
         [(? word? name) name]
         [(? id? name) `(list ,name)]
         [(? object:defn? obj) `(list ,(expr-of obj #:class class))]
         [(? object:sugar? obj) `(list ,(expr-of obj #:class class))]
         [_ `(FIXME '(Objects ,objs))]))]
    [(object-set:from-object object field) (do-field-ref object field)]

    ;; ----------------------------------------
    ;; CONSTRAINT
    [(constraint:single-value value) `(equal? ,(expr-of value))]
    [(constraint:includes type) `(includes? ,(expr-of type))]
    [(constraint:value-range lo hi)
     (define (endpoint x) (case x [(MIN) 'MIN] [(MAX) 'MAX] [else (expr-of x)]))
     `(range ,(endpoint lo) ,(endpoint hi))]
    [(constraint:size c) `(size ,(expr-of c))]
    [(constraint:pattern v) `(pattern ,(expr-of v))]
    [(constraint:inner-type (? list? tcs))
     `(inner ,@(map (match-lambda [(ast:named name cc) `[,name ,(expr-of cc)]]) tcs))]
    [(constraint:inner-type c) `(inner ,(expr-of c))]
    [(constraint:component vc pc)
     `(component ,(and vc (expr-of vc)) ,pc)]
    [(constraint:containing type)
     `(containing ,(expr-of type))]
    [(constraint:containing/encoded-by type value)
     `(containing/encoded-by ,(and type (expr-of type)) ,(expr-of value))]
    [(constraint:or c1 c2) `(union ,(expr-of c1) ,(expr-of c2))]
    [(constraint:and c1 c2) `(intersect ,(expr-of c1) ,(expr-of c2))]
    [(constraint:user) `(user-constraint)]
    [(constraint:table objset ats)
     `(table-constraint ,(expr-of objset) ,(if ats `'(@ ,@ats) #f))]

    ;; ----------------------------------------
    ;; GENERIC
    [(? id? s) s]
    [(? word? s) s]
    [(? exact-integer? n) n]
    [(ref:dot modref ref)
     (string->symbol (format "~s.~s" modref ref))]
    [(expr:apply thing args)
     `(,(expr-of thing) ,@(map expr-of args))]
    [(expr:fun params thing)
     `(lambda ,(map formal-of params) ,(expr-of thing))]

    ;; ----------------------------------------
    [_ `(FIXME '(expr ,x))]))

(define (do-field-ref obj field)
  (foldr (lambda (f o) `(hash-ref ,o ',f)) (expr-of obj) field))

(define (decls-for-type t [tname #f])
  (define (make-def x)
    (match-define (ast:named name value) x)
    `(define ,name ,(expr-of value)))
  (define (format-symbol fmt . args)
    (string->symbol (apply format fmt args)))
  (match t
    [(type:integer (? list? names))
     (map make-def names)]
    [_ null]))

(define (seq/set-field-of f)
  (match f
    [(opt:optional (ast:named name type))
     `[,name ,@(splice-expr-of type) #:optional]]
    [(opt:default (ast:named name type) default)
     `[,name ,@(splice-expr-of type) #:default ,(expr-of default)]]
    [(ast:named name type)
     `[,name ,@(splice-expr-of type)]]))

(define (choice-alt-of a)
  (match a
    [(ast:named name type)
     `[,name ,@(splice-expr-of type)]]))

(define (splice-expr-of t)
  (match t
    [(? type:tagged?)
     (match (expr-of t)
       [`(TAG . ,contents) contents])]
    [t (list (expr-of t))]))

(define (const-oid-component? c)
  (or (exact-nonnegative-integer? c) (ast:named? c)))

(define (sexpr-of-oid-component c)
  (match c
    [(? exact-nonnegative-integer? n) n]
    [(ast:named name num) `(,name ,num)]))

(define (expr-of-oid-component c)
  (match c
    [(? id? name) name]
    [(? exact-nonnegative-integer? n) n]
    [(ast:named name num) num]))

(define (sexpr-of-class-field f)
  (define (opt-of o)
    (match o
      [(opt:optional _) '[#:optional]]
      [(opt:default _ thing) `(#:default ,(expr-of thing))]
      [#f '[]]))
  (match f
    [(field:type ref opt)
     `[#:type ,ref ,@(opt-of opt)]]
    [(field:value/fixed-type ref type uniq opt)
     `[#:value ,ref ,(expr-of type) ,@(if uniq '(#:unique) '()) ,@(opt-of opt)]]
    [(field:value/var-type ref type opt)
     `[#:value ,ref #:dependent ,(expr-of type) ,@(opt-of opt)]]
    [(field:value-set/fixed-type ref type opt)
     `[#:value-set ,ref ,(expr-of type) ,@(opt-of opt)]]
    [(field:value-set/var-type ref type opt)
     `[#:value-set ,ref #:dependent ,(expr-of type) ,@(opt-of opt)]]
    [(field:object ref class opt)
     `[#:object ,ref ,(expr-of class) ,@(opt-of opt)]]
    [(field:object-set ref class opt)
     `[#:object-set ,ref ,(expr-of class) ,@(opt-of opt)]]
    ))

(define (make-append xse yse)
  (match* [xse yse]
    [[`(list ,@xs) `(list ,@ys)]
     `(list ,@xs ,@ys)]
    [[`(append ,@xsse) yse]
     `(append ,@xsse ,yse)]
    [[xse yse] `(append ,xse ,yse)]))

(define (comments . cs)
  (map (lambda (c) (unquoted-printing-string (format "#| ~a |#" c))) cs))

(define (begin/comment c expr)
  `(begin ,@(comments c) ,expr))

(define (begin/fixme fixme expr)
  (if fixme
      (case (current-fixme-mode)
        [(fixme) `(begin (FIXME ',fixme) ,expr)]
        [(quote) `(begin '(FIXME ,fixme) ,expr)]
        [(comment) `(begin ,@(comments (format "FIXME ~s" fixme)) ,expr)]
        [(omit) expr])
      expr))

(define (unbegin form)
  (match form
    [(cons 'begin forms) forms]
    [form (list form)]))

(define (mark-duplicates forms)
  (define seen (make-hasheq))
  (define (handle-form form)
    (match form
      [`(define ,name ,rhs)
       (match (hash-ref seen name #f)
         [(list (== rhs))
          (list (unquoted-printing-string (format "#;~s" form)))]
         [(list other-rhs)
          (list (unquoted-printing-string (format "#;~s #| CONFLICT |#" form)))]
         [#f
          (hash-set! seen name (list rhs))
          (list form)])]
      [`(begin ,@forms)
       (list `(begin ,@(handle-forms forms)))]
      [form (list form)]))
  (define (handle-forms forms)
    (apply append (map handle-form forms)))
  (handle-forms forms))


;; ============================================================

(module+ main
  (require racket/cmdline
           racket/pretty
           "grammar.rkt"
           "typecheck.rkt")

  (define (pretty-print-code code)
    (parameterize ((pretty-print-columns 96)
                   (pretty-print-current-style-table
                    (pretty-print-extend-style-table
                     (pretty-print-current-style-table)
                     '(define-asn1-type)
                     '(define))))
      (pretty-write code)))

  (command-line
   #:once-any
   [("--omit-fixme") "Omit FIXME annotations" (current-fixme-mode 'omit)]
   #:args files
   (printf "#lang racket/base\n")
   (for ([file files])
     (call-with-input-file* file
       (lambda (in)
         (port-count-lines! in)
         (define mod0 (read-module in))
         (define mod1 (typecheck mod0))
         (define mod2 (apply-tagging-mode mod1))
         (printf "\n;; Translation of ~s\n" file)
         (for-each pretty-print-code (unbegin (decl-of-module mod2))))))))