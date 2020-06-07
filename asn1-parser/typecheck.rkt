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
               [(word? sym) (env-add! sym 'type #f)])))
     h]))

(define (tc-definition def)
  (with-handlers ([tcfail? (lambda (e)
                             (eprintf "FAILED: ~e\n" e)
                             (ambiguous (list def)))])
    (tc-definition* def)))

(define (tc-definition* def)
  (match def
    [(assign:word name params rhs)
     (match (ast-kind* rhs)
       ['type
        (define ast (tc-type (maybe-fun params rhs)))
        (env-add! name 'type ast)
        (assign:type name ast)]
       ['class
        (define ast (maybe-fun params rhs))
        (env-add! name 'class ast)
        (assign:class name ast)]
       [k (fail 'def k def)])]
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
       [k (fail 'def k def)])]
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
    [(list v) v]
    [vs* (fail kind t (ambiguous vs*))]))

(define (tc-type t)
  (match t
    [(? (ref-of-kinds? '(type)) ref) ref]
    [(ambiguous vs) (disambiguate 'type (lambda (_) tc-type) #f t)]
    [(expr:apply fun args)
     ;; FIXME
     (tc-type fun)]
    [(expr:fun params body)
     (parameterize ((env (env-add-params (env) params)))
       (tc-type body))]
    [(type:integer nvs)
     (begin0 t
       (for ([nv (in-list nvs)])
         (match nv [(ast:named name value) (env-add! name 'value t)])))]
    [(app ast-kind 'type) t]
    [_ (fail 'type #f t)]))

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
     ((tc-value type) value)]
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
      [(field:type (== name) _) tc-type]
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
        (object:defn (map (tc-class-component cs) vs))]
       [_ (fail 'object t v)])]
    [(object:sugar sugar)
     (match (ast-eval t)
       [(class:defn cs pattern)
        (let ([vs (or (desugar sugar pattern) (fail 'object t v))])
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


;; ============================================================
;; Desugaring

(define (desugar sugar pattern)
  (let loop ([sugar sugar] [pattern pattern] [acc null])
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

(module+ main
  (require racket/pretty
           racket/cmdline
           "ggramar2.rkt")

  (define (initial-pass defs)
    (for/list ([def (in-list defs)])
      (when #f
        (eprintf "type checking:\n")
        (pretty-print def)
        (eprintf "=>\n"))
      (define ddef (tc-definition def))
      (pretty-print ddef)
      ddef))

  (define (disambiguation-pass defs)
    (for/list ([def (in-list defs)])
      (match def
        [(ambiguous (list def))
         (when #f
           (eprintf "disambiguating:\n")
           (pretty-print def)
           (eprintf "=>\n"))
         (define ddef (tc-definition def))
         (cond [(ambiguous? ddef)
                (eprintf "...still ambiguous\n")]
               [else (pretty-print ddef)])
         ddef]
        [def def])))

  (define (print-env-delta env old-env)
    (define env-delta
      (for/fold ([env env]) ([k (in-hash-keys old-env)])
        (hash-remove env k)))
    (sort (hash-map env-delta list) symbol<? #:key car))

  (define (check-ambiguous def)
    (match def
      [(ambiguous (list def))
       (when #t
         (printf "ambiguity:\n")
         (pretty-print def))
       (tc-definition* def)]
      [_ (void)]))

  (command-line
   #:args files
   (for ([file files])
     (call-with-input-file* file
       (lambda (in)
         (port-count-lines! in)
         (define header (read-module-header in))
         (pretty-print (tc-header header))
         (define defs0 (read-assignments in))
         (define defs
           (for/fold ([defs (initial-pass defs0)])
                     ([i (in-range 10)] #:when (ormap ambiguous? defs))
             (eprintf "-- DISAMBIGUATION PASS ~s --\n" i)
             ;;(begin (printf "Ready?\n") (void (read-line)))
             (disambiguation-pass defs)))
         (when #t (for-each check-ambiguous defs))
         (let ([amb (count ambiguous? defs)])
           (cond [(zero? amb) (eprintf "Success.\n")]
                 [else (eprintf "Failed to eliminate ~s ambiguities.\n" amb)])))))))
