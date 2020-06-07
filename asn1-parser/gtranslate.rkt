#lang racket/base
(require racket/match
         racket/list
         "ast2.rkt"
         "tree-util.rkt")

;; Other ideas
;; - topological sort definitions by dependence?

;; Pre-pass
;; - track type/class of context
;; - desugar information objects
;; - mark dependent SET/SEQUENCE fields
;; - resolve tagging mode to implicit/explicit
;; - disambiguate NULL type vs NULL value (eg in object)
;; - fix OID parsed as seq/set value, eg { id-pkix 1 } (or vice versa?)

(struct tcfail (kind v t) #:prefab)
(define (fail kind v t) (raise (tcfail kind v t)))

(define (map-ok f xs)
  (for/fold ([acc null] #:result (reverse acc))
            ([x (in-list xs)])
    (define v (with-handlers ([tcfail? (lambda (e) #f)]) (list (f x))))
    (if (pair? v) (cons (car v) acc) acc)))

;; ============================================================
;; Pass 1: type-based disambiguation

;; Ambiguities
;; - NULL = value or type?
;; - { id num } = OID/RelOID (2 parts) or SEQ/SET (1 name+value)
;; - { id { value } } = SEQ/SET-OF{ call id on value } or SEQ/SET{ id := { value } SET/SEQ-OF }
;; - Word ::= Word = type assignment or class assignment?

;; env : Parameter of Hash[Symbol => EnvEntry]
;; EnvEntry = (cons 'type Type) | (cons 'class Class) | ...
(define env (make-parameter base-env))

(define (env-add! name kind rhs)
  (when (memq kind '(type class))
    (eprintf "!! adding ~s : ~s\n" name kind))
  (env (hash-set (env) name (cons kind rhs))))

(define (env-add-params env params)
  (for/fold ([env env]) ([p (in-list params)])
    (match-define (param gov name) p)
    (hash-set env name
              (cond [(eq? (ast-kind* gov) 'type)
                     (cond [(id? name) (cons 'value gov)]
                           [else (cons 'value-set gov)])]
                    [(eq? (ast-kind* gov) 'class)
                     (cond [(id? name) (cons 'object gov)]
                           [else (cons 'object-set gov)])]
                    [else '(#f)]))))

(define ((ref-of-kinds? kinds) v)
  (and (symbol? v) (memq (car (hash-ref (env) v '(#f))) kinds)))

(define (ast-kind* v)
  (match v
    [(? symbol? name) (car (hash-ref (env) name '(#f)))]
    [(expr:apply f args) (ast-kind* f)]
    [(ambiguous vs)
     (define kinds (map ast-kind* vs))
     (if (for/and ([kind (in-list kinds)]) (eq? kind (car kinds))) (car kinds) #f)]
    [_ (ast-kind v)]))

(define (ast-eval v)
  (match v
    [(? symbol? name)
     (match (hash-ref (env) name #f)
       [(cons _ rhs) (ast-eval rhs)]
       [else #f])]
    [(expr:apply fun args)
     (match (ast-eval fun)
       [(expr:fun params body)
        (unless (= (length args) (length params))
          (fail 'eval #f v))
        (define venv
          (for/fold ([venv (hasheq)]) ([p (in-list params)] [a (in-list args)])
            (match-define (param gov name) p)
            ;; FIXME: check arg against gov
            (hash-set venv name a)))
        (ast-eval (substitute venv body))])]
    ;; ....
    [v v]))

(define (substitute venv ast)
  (define (handle-ast ast recur)
    (match ast
      [(? symbol? ref)
       (cond [(hash-ref venv ref #f) => values] [else ref])]
      ;; A few AST nodes use symbols in non-ref position.
      [(type name) ast]
      [(value v) ast]
      [(ref:dot modref ref) ast]
      [(ast:named name thing) (ast:named name (recur thing))]
      ;; All others, just recur
      [ast (recur ast)]))
  (tree-transform-preorder ast handle-ast))

(define (type->vtype t)
  (match (ast-eval t)
    [(type:tagged _ _ t) (type->vtype t)]
    [(type:constrained t _) (type->vtype t)]
    [t (cond [(eq? (ast-kind t) 'type) t] [else #f])]))

(define (tc-header h)
  (match h
    [(mod:defn id tagmode extmode exports imports assignments)
     (for ([imp (in-list imports)])
       (for ([sym (in-list (mod:import-syms imp))])
         (cond [(id? sym) (env-add! sym 'value #f)]
               [(word? sym) (env-add! sym 'type #f)])))]))

(define (tc-definition def)
  (with-handlers ([tcfail? (lambda (e) (ambiguous (list def)))])
    (tc-definition* def)))

(define (tc-definition* def)
  ;;(eprintf "processing ~e\n" def)
  (match def
    [(assign:word name params rhs)
     (match (ast-kind* rhs)
       ['type
        (define ast (maybe-fun params rhs))
        (env-add! name 'type ast)
        (assign:type name ast)]
       ['class
        (define ast (maybe-fun params rhs))
        (env-add! name 'class ast)
        (assign:class name ast)]
       [_ (fail 'def 'word def)])]
    [(assign:id name params kind rhs)
     (match (ast-kind* kind)
       ['type
        (define ast ((tc-value kind) (maybe-fun params rhs)))
        (env-add! name 'value ast)
        (assign:value name kind ast)]
       ['class
        (define ast ((tc-object kind) (maybe-fun params rhs)))
        (env-add! name 'object ast)
        (assign:object name kind ast)]
       [_ (fail 'def 'id def)])]
    [(assign:x-set name params kind rhs)
     (match (ast-kind* kind)
       ['type
        (define ast (tc-value-set kind (maybe-fun params rhs)))
        (env-add! name 'value-set ast)
        (assign:value-set name kind ast)]
       ['class
        (define ast ((tc-object-set kind) (maybe-fun params rhs)))
        (env-add! name 'object-set ast)
        (assign:object-set name kind ast)]
       [_ (fail 'def 'x-set def)])]
    [#f #f]))

(define (disambiguate kind tc t v)
  (match-define (ambiguous vs) v)
  (match (map-ok (tc t) vs)
    [(list v)
     (eprintf "DISAMBIGUATE OK\n")
     v]
    [vs*
     (eprintf "DISAMBIGUATE ~s => ~s\n" (length vs) (length vs*))
     (fail kind t (ambiguous vs*))]))

(define ((tc-value t) v)
  (define (get-vtype) (type->vtype (ast-eval t)))
  (match v
    [(? (ref-of-kinds? '(value))) v]
    [(ambiguous vs) (disambiguate 'value tc-value t v)]
    [(expr:fun params body)
     (parameterize ((env (env-add-params (env) params)))
       ((tc-value t) body))]
    [(? value:oid/reloid?)
     (match (get-vtype)
       [(or (type 'OBJECT-IDENTIFIER) (type 'RELATIVE-OID)) v]
       [(or (? type:set?) (? type:sequence?))
        (fail 'value v t)]
       [_ v])]
    [(value:seq/set-of vs)
     (match (get-vtype)
       [(type 'BIT-STRING)
        (error 'tc-value "fixme: bit string")]
       [(type:sequence-of subt _)
        (value:seq/set-of (map (tc-value subt) vs))]
       [(type:set-of subt _)
        (value:seq/set-of (map (tc-value subt) vs))]
       [(or (type 'OBJECT-IDENTIFIER) (type 'RELATIVE-OID))
        (fail 'value v t)]
       [_ v])]
    [(value:seq/set vs)
     (match (get-vtype)
       [(or (type:sequence components) (type:set components))
        (value:seq/set (map (tc-component components) vs))]
       [(or (type 'OBJECT-IDENTIFIER) (type 'RELATIVE-OID))
        (fail 'value v t)]
       #;[_ (fail 'value v t)]
       [_ v])]
    [(value:choice name value)
     (match (get-vtype)
       [(type:choice alts)
        (define alttype (lookup-named name alts))
        ((tc-value alttype) value)]
       [_ (fail 'value v t)])]
    [(value:annotated type value)
     ((tc-value value) type)]
    [(value _) v]
    [(value:from-object object field) v]
    [(value:bstring s) v]
    [(value:hstring s) v]
    [(value:bit-list bits) v]
    [_ (fail 'value t v)]))

(define (lookup-named name nvs)
  (for/or ([nv (in-list nvs)])
    (let loop ([nv nv])
      (match nv
        [(ast:named (== name) value) value]
        [(ast:named _ _) #f]
        [(opt:optional nv) (loop nv)]
        [(opt:default nv _) (loop nv)]))))

(define ((tc-class-component cs) nv)
  (match-define (ast:named name value) nv)
  (define check-thing (lookup-field name cs))
  (unless check-thing (fail 'class-component cs nv))
  (check-thing value))

(define (lookup-field name cs)
  (for/or ([c (in-list cs)])
    (match c
      [(field:type (== name) _)
       (lambda (t)
         #;(eprintf "checking ok type: ~e, ~e\n" t (ast-kind* t))
         (case (ast-kind* t) [(type) t] [else (fail 'type #f t)]))]
      [(field:value/fixed-type (== name) type _ _) (tc-value type)]
      [(field:value/var-type (== name) _ _) (tc-value #f)]
      [(field:value-set/fixed-type (== name) type _) (tc-value-set type)]
      [(field:value-set/var-type (== name) _ _) (tc-value-set #f)]
      [(field:object (== name) type _) (tc-object type)]
      [(field:object-set (== name) type _) (tc-object-set type)]
      [_ #f])))

(define ((tc-component components) nv)
  (match-define (ast:named name v) nv)
  (define type (lookup-named name components))
  ((tc-value type) v))

(define ((tc-value-set t) v)
  (match v
    [(? (ref-of-kinds? '(value-set))) v]
    [(ambiguous vs) (disambiguate 'value-set tc-value-set t v)]
    [(x-set:defn vs)
     (value-set:defn (tc-element-set 'value t vs))]
    [(value-set:defn vs)
     (value-set:defn (tc-element-set 'value t vs))]
    #;[(value-set:from-object _ _) _]
    [_ (fail 'value-set t v)]))

(define (tc-element-set kind t es)
  (let loop ([es es])
    (match es
      [(? null?) null]
      [(constraint:or es1 es2)
       (constraint:or (loop es1) (loop es2))]
      [(constraint:and es1 es2)
       (constraint:and (loop es1) (loop es2))]
      [es
       (case kind
         [(value)
          (match es
            ;; SubtypeElements -- FIXME
            [(constraint:single-value v)
             (constraint:single-value ((tc-value t) v))]
            [(constraint:includes t) es]
            [(constraint:value-range lo hi) es]
            [(constraint:size c) es]
            [(constraint:pattern v) es]
            [(constraint:inner-type _) es]
            [(constraint:component _ _) es]
            [es
             (case (ast-kind* es)
               [(value) (value-set:one ((tc-value t) es))]
               [(value-set) ((tc-value-set t) es)]
               [else (fail 'value t es)])])]
         [(object)
          (case (ast-kind* es)
            [(object) (object-set:one ((tc-object t) es))]
            [(object-set) ((tc-object-set t) es)]
            [else (fail 'object t es)])])])))

(define ((tc-object t) v)
  (match v
    [(? (ref-of-kinds? '(object)) ref) ref]
    [(ambiguous vs) (disambiguate 'object tc-object t v)]
    [(object:defn vs)
     (match (ast-eval t)
       [(class:defn cs _)
        (eprintf "CHECKING:\n~v\n~v\n" cs vs)
        (with-handlers ([tcfail? (lambda (e) (eprintf "FAILED: ~e\n\n" e) (raise e))])
          (begin0 (object:defn (map (tc-class-component cs) vs))
            (eprintf "OK!\n\n")))]
       [_ (fail 'object t v)])]
    [(object:sugar sugar)
     (match (ast-eval t)
       [(class:defn cs pattern)
        (let ([vs (or (desugar sugar pattern)
                      (and (eprintf "DESUGAR FAILED:\n~v\n~v\n\n" sugar pattern)
                           (fail 'object t v)))])
          (eprintf "DESUGARED TO:\n~v\n" vs)
          ((tc-object t) (object:defn vs)))]
       [_ (fail 'object t v)])]
    [_ (fail 'object t v)]))

(define ((tc-object-set t) v)
  (match v
    [(? (ref-of-kinds? '(object-set)) ref) ref]
    [(ambiguous vs) (disambiguate 'object-set tc-object-set t v)]
    [(x-set:defn vs)
     (object-set:defn (tc-element-set 'object t vs))]
    [(value-set:defn vs)
     (object-set:defn (tc-element-set 'object t vs))]
    #;[(object-set:from-object _ _) _]
    [_ (fail 'object-set t v)]))


#;
(define (eval-classifier v)
  (match v
    [(? symbol? name)
     (match (hash-ref (env) v #f)
       [(cons 'type rhs) (eval-classifier rhs)]
       [(cons 'class rhs) (eval-classifier rhs)]
       [_ #f])]
    [(type:tagged _ _ ty) (eval-classifier ty)]
    [(type:constrained ty _) (eval-classifier ty)]
    [term
     (match (ast-kind* term)
       ['type (cons 'type term)]
       ['class (cons 'class term)]
       [_ #f])]))

#;
(define (ok-definition? def)
  ;; Reject obvious type errors to disambiguate.
  (match def
    [(assign:id name params kind rhs)
     (match (eval-classifier kind)
       [(cons 'type ty) (ok-value? rhs ty)]
       [(cons 'class c) (ok-object? rhs c)]
       [#f #t])]
    [(assign:x-set name params kind rhs)
     (match (eval-classifier kind)
       [(cons 'type ty) (ok-value-set? rhs ty)]
       [(cons 'class c) (ok-object-set? rhs c)]
       [#f #t])]
    [(assign:word name params rhs) #t]
    [#f #f]))

#;
(define (ok-value? v ty)
  (and
   (memq (detect-term-kind v) '(value #f)) ;; eg, NULL value vs type
   (match ty
     [(or (type 'OBJECT-IDENTIFIER) (type 'RELATIVE-OID))
      (match v
        [(or (? value:seq/set?) (? value:seq/set-of?)) #f]
        [_ #t])]
     [(or (? type:sequence?) (? type:set?))
      (match v
        [(or (? value:oid/reloid?)) #f]
        [_ #t])]
     [(or (? type:sequence-of?) (? type:set-of?))
      (match v
        [(or (? value:oid/reloid?)) #f]
        [_ #t])]
     [_ #t])))

#;
(define (ok-object? v c)
  (and (memq (detect-term-kind v) '(object #f))))

#;#;
(define (ok-value-set? v ty)
  (and (memq (detect-term-kind v) '(value-set x-set #f))))
(define (ok-object-set? v ty)
  (and (memq (detect-term-kind v) '(object-set x-set #f))))

;; ============================================================


#;
(define (template v)

  (define (for-thing x)
    (match x

      ;; ----------------------------------------
      ;; Module

      [(mod:defn id tagmode extmode exports imports assignments) _]
      [(mod:id name oid) _]
      [(mod:import syms modid) _]

      ;; ----------------------------------------
      ;; Assignment
      [(assign:type name params type) _]
      [(assign:value name params type value) _]
      [(assign:value-set name params type value-set) _]
      [(assign:class name params class) _]
      [(assign:object name params class object) _]
      [(assign:object-set name params class object-set) _]

      ;; ----------------------------------------
      ;; TYPE
      [(ref:type name) _]
      [(type name) _]
      [(type:bit-string _) _]
      [(type:choice alts) _]
      [(type:enum _)  _]
      [(type:integer _) _]
      [(type:sequence fields) _]
      [(type:set fields) _]
      [(type:sequence-of type size-c) _]
      [(type:set-of type size-c) _]
      [(type:string subtype) _]
      [(type:tagged (tag tagclass tagnum) mode type) _]
      ;; ----
      [(type:constrained (type:from-class class fields) (constraint:table objset ats)) _]
      [(type:constrained type constraint) _]
      [(type:any-defined-by id) _]
      [(type:from-object object field) _]
      [(type:from-class class field) _]
      [(type:instance-of oid) _]
      [(type:select id type) _]

      ;; ----------------------------------------
      ;; VALUE
      [(ref:value name) _]
      [(value v) _]
      [(value:bstring s) _]
      [(value:hstring s) _]
      [(value:annotated type value) _]
      [(value:bit-list bits) _]
      [(value:choice name value) _]
      [(value:oid/reloid cs) _]
      [(value:seq/set-of values) _]
      [(value:seq/set values) _]
      [(value:from-object object field) _]

      ;; ----------------------------------------
      ;; VALUE SET
      [(value-set:defn vs) _]
      [(value-set:from-object object field) _]

      ;; ----------------------------------------
      ;; CLASS
      [(ref:class name) _]
      [(class:defn fields _) _]
      [(class:type-identifier) _]

      ;; ----------------------------------------
      ;; OBJECT
      [(ref:object name) _]
      [(object:defn decls) _]
      [(object:sugar sugar) _]
      [(object:from-object object field) _]

      ;; ----------------------------------------
      ;; OBJECT SET
      [(ref:object-set name) _]
      [(object-set:defn objs) _]
      [(object-set:from-object object field) _]

      ;; ----------------------------------------
      ;; CLASS FIELD
      [(field:type ref opt) _]
      [(field:value/fixed-type ref type uniq opt) _]
      [(field:value/var-type ref type opt) _]
      [(field:value-set/fixed-type ref type opt) _]
      [(field:value-set/var-type ref type opt) _]
      [(field:object ref class opt) _]
      [(field:object-set ref class opt) _]

      ;; ----------------------------------------
      ;; CONSTRAINT
      [(constraint:single-value value) _]
      [(constraint:includes type) _]
      [(constraint:value-range lo hi) _]
      [(constraint:size c) _]
      [(constraint:pattern v) _]
      [(constraint:inner-type (? list? tcs)) _]
      [(constraint:inner-type c) _]
      [(constraint:component vc pc) _]
      [(constraint:containing type) _]
      [(constraint:containing/encoded-by type value) _]
      [(constraint:or c1 c2) _]
      [(constraint:and c1 c2) _]
      [(constraint:user) _]
      [(constraint:table objset ats) _]

      ;; ----------------------------------------
      ;; GENERIC
      [(ref:dot modref ref) _]
      [(expr:apply thing args) _]
      [(ast:named name thing) _]
      [(param governor ref) _]
      [(opt:optional thing) _]
      [(opt:default thing default) _]

      ;; ----------------------------------------
      ))

  (void))

;; ============================================================
;; Desugaring

#;
(define (desugar-object sugar classref)
  (match (lookup-class classref)
    [(class:defn cs pattern)
     (desugar sugar pattern)]
    [v #f]))

(define (desugar sugar pattern)
  (let loop ([sugar sugar] [pattern pattern] [acc null])
    #;(eprintf "desugar:\n ~v\n ~v\n ~v\n\n" sugar pattern acc)
    (match* [sugar pattern]
      [[(cons lit sugar) (cons lit pattern)] ;; nonlinear!
       (loop sugar pattern acc)]
      [[sugar (cons (sugar:optional group) pattern)]
       (or (loop sugar (append group pattern) acc)
           (loop sugar pattern acc))]
      [[(cons value sugar) (cons (? field-ref? ref) pattern)]
       (loop sugar pattern (cons (ast:named ref value) acc))]
      [['() '()]
       (reverse acc)]
      [[_ _] #f])))

(define (field-ref? x)
  (or (&id? x) (&word? x)))

;; ============================================================
;; Translation

#|
(define current-fixme-mode (make-parameter 'comment))
(define current-tag-mode (make-parameter #f))
(define current-env (make-parameter (hash)))

(define (decl-of-module m)
  (match m
    [(mod:defn id tagmode extmode exports imports assignments)
     ;; FIXME: id, tagmode, extmode, exports, imports
     (define env (env-of-assignments assignments))
     (parameterize ((current-env env)
                    (current-tag-mode
                     (case tagmode
                       [(implicit) 'implicit]
                       [(automatic) 'automatic]
                       [(explicit #f) 'explicit])))
       (decl-of-assignments assignments))]))

(define (lookup-class ref)
  (define env (current-env))
  (let loop ([ref ref])
    (match (hash-ref env ref #f)
      [(assign:class _ _ (? class:defn? defn)) defn]
      [_ #f])))

(define (env-of-assignments assignments)
  (for/fold ([env #hash()]) ([assignment (in-list assignments)])
    (match assignment
      [(assign:class name params class) (hash-set env name assignment)]
      [_ env])))

(define (decl-of-assignments assignments)
  (cons 'begin (map decl-of-assignment assignments)))

(define (decl-of-assignment a)
  (define (header-of name params)
    (cond [(null? params) name]
          [else (cons name (map formal-of params))]))
  (define (do-begin a bs) (if (null? bs) a `(begin ,a ,@bs)))
  (match a
    [(assign:type name params type)
     (do-begin
      `(define ,(header-of name params) ,(expr-of type))
      (decls-for-type type))]
    [(assign:value name params type value)
     `(define ,(header-of name params)
        ,@(comments (format "~s" (expr-of type)))
        ,(expr-of value))]
    [(assign:value-set name params type value-set)
     `(define ,(header-of name params)
        ,@(comments (format "~s" (expr-of type)))
        ,(expr-of value-set))]
    [(assign:class name params class)
     `(define ,(header-of name params) ,(expr-of class))]
    [(assign:object name params class object)
     `(define ,(header-of name params)
        ,@(comments (format "~s" (expr-of class)))
        ,(expr-of object #:class class))]
    [(assign:object-set name params class object-set)
     `(define ,(header-of name params)
        ,@(comments (format "~s" (expr-of class)))
        ,(expr-of object-set #:class class))]))

(define (formal-of p)
  (match p [(param gov ref) ref]))

(define (expr-of x #:class [class #f])
  (match x

    ;; ----------------------------------------
    ;; TYPE
    #;[(ref:type name) name]
    [(type name) name]
    [(type:bit-string _) 'BIT-STRING]
    [(type:choice alts) `(CHOICE ,@(map choice-alt-of alts))]
    [(type:enum _) 'ENUMERATED]
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
     (begin/fixme
       (and (not mode) (not (eq? (current-tag-mode) 'explicit)) "check tag mode")
       `(TAG ,(match tagclass
                ['universal '#:universal]
                ['application '#:application]
                ['context-sensitive '#:context-sensitive]
                ['private '#:private])
             ,@(match mode
                 ['implicit '(#:implicit)]
                 ['explicit '(#:explicit)]
                 [#f (case (current-tag-mode)
                       [(explicit) '(#:explicit)]
                       [(implicit) '(#:implicit)] ;; FIXME, not for open types
                       [(automatic) '(#:FIXME)])])
             ,tagnum
             ,(expr-of type)))]
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
        `(build-OID base-oid ,@(map sexpr-of-oid-component cs))]
       [(cons (? id? base-oid) cs)
        `(append base-oid (list ,@(map expr-of-oid-component cs)))]
       [(? const-oid? cs)
        `(OID ,@(map sexpr-of-oid-component cs))]
       [cs
        `(list ,@(map expr-of-oid-component cs))])]
    [(value:seq/set-of values)
     `(list ,@(for/list ([value (in-list values)]) (expr-of value)))]
    [(value:seq/set values)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              values)))]
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
    [(class:type-identifier)
     `TYPE-IDENTIFIER]

    ;; ----------------------------------------
    ;; OBJECT
    #;[(ref:object name) name]
    [(object:defn decls)
     `(hasheq ,@(append* (map (match-lambda
                                [(ast:named name value)
                                 (list `(quote ,name) (expr-of value))])
                              decls)))]
    [(object:sugar sugar)
     (cond [(desugar-object sugar class)
            => (lambda (decls)
                 (expr-of (object:defn decls)))]
           [else `(FIXME '(Sugar ,sugar ,class ,(lookup-class class)))])]
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

    ;; ----------------------------------------
    [_ `(FIXME '(expr ,x))]))

(define (do-field-ref obj field)
  `(object-ref ,(expr-of obj) ',(append (car field) (list (cdr field)))))

(define (decls-for-type t)
  (define (make-def x)
    (match-define (ast:named name value) x)
    `(define ,name ,(expr-of value)))
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
     `[,name ,(expr-of type) #:default ,(expr-of default)]]
    [(ast:named name type)
     `[,name ,(expr-of type)]]))

(define (choice-alt-of a)
  (match a
    [(ast:named name type)
     `[,name ,(expr-of type)]]))

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
|#

;; ============================================================

(module+ main
  (require racket/pretty
           racket/cmdline
           racket/class
           grrparse
           "glexer.rkt"
           "ggramar2.rkt")

  (define (read-definitions in)
    (let loop ()
      (define rs (send asn1-assignment-parser parse* (asn1-lexer in)))
      (define drs (remove-duplicates (map simplify-collect-boxes rs)))
      (eprintf "\n-- ~s => ~s --\n" (length rs) (length drs))
      (match drs
        [(list (token 'AssignmentOrEnd #f))
         null]
        [(list (token 'AssignmentOrEnd def))
         (when #t
           (eprintf "type-checking and disambiguating:\n")
           (pretty-print def)
           (eprintf "=>\n"))
         (define ddef (tc-definition def))
         (pretty-print ddef)
         (cons ddef (loop))]
        [_ (error 'read-definitions "results (~s): ~e" (length drs) drs)])))

  (define (dpass defs)
    (when #f
      (pretty-print (sort (for/list ([(k v) (in-hash (env))] #:when v) (list k v))
                          symbol<? #:key car)))
    (for/list ([def (in-list defs)])
      (match def
        [(ambiguous (list def))
         (when #t
           (eprintf "disambiguating:\n")
           (pretty-print def)
           (eprintf "=>\n"))
         (define ddef (tc-definition def))
         (cond [(ambiguous? ddef)
                (eprintf "!! still ambiguous !!\n")]
               [else (pretty-print ddef)])
         ddef]
        [def def])))

  (command-line
   #:once-any
   #;[("-a") "Parse an assignment list" (set! the-parser asn1-assignments-parser)]
   #;[("-m") "Parse a single module (default)" (void)]
   #:args files
   (for ([file files])
     (call-with-input-file* file
       (lambda (in)
         (port-count-lines! in)
         (match (send asn1-module-header-parser parse* (asn1-lexer in))
           [(list (token _ header))
            (pretty-print header)
            (tc-header header)])
         (define defs (read-definitions in))
         (define ddefs
           (for/fold ([defs defs]) ([i (in-range 10)] #:when (ormap ambiguous? defs))
             (eprintf "-- DISAMBIGUATION PASS ~s --\n" i)
             (dpass defs)))
         (for ([def (in-list ddefs)])
           (match def
             [(ambiguous (list def))
              (when #t
                (printf "...\n")
                (pretty-print def))
              (tc-definition* def)]
             [_ (void)]))
         (for-each tc-definition* ddefs)
         (cond [(ormap ambiguous? ddefs)
                (eprintf "Ambiguities remain.\n")]
               [else
                (eprintf "Success.\n")]))))))

