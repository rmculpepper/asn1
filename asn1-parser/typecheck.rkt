#lang racket/base
(require racket/match
         racket/list
         racket/pretty
         "ast2.rkt"
         "tree-util.rkt")
(provide (all-defined-out))

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

;; module-env : Parameterof Hasheq[Symbol => (cons ModDefn Env)]
(define module-env (make-parameter (hasheq)))

(define (module-env-add! mod modenv)
  (module-env (hash-set (module-env) (mod:id-name (mod:defn-id mod)) (cons mod modenv))))

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
  (when #f #;(memq kind '(type class))
    (eprintf "!! adding ~s : ~s\n" name kind))
  (env (hash-set (env) name (cons kind rhs))))

(define (env-add-params env params)
  (for/fold ([env env]) ([p (in-list params)])
    (match-define (param gov name) p)
    (hash-set env name
              (cond [(eq? gov #f)
                     (cond [(word? name) (cons 'type (type:param))]
                           [else '(#f)])]
                    [(eq? (ast-kind* gov) 'type)
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
       (match-define (mod:import syms (mod:id modid _)) imp)
       (define menv (cond [(hash-ref (module-env) modid #f) => cdr] [else '#hash()]))
       (for ([sym (in-list syms)] #:when (not (hash-has-key? (env) sym)))
         (cond [(and menv (hash-ref menv sym #f))
                => (match-lambda
                     [(cons 'type t)
                      (define definite (parameterize ((env menv)) (type-definite sym)))
                      (env-add! sym 'type (type:imported sym definite))]
                     [(cons kind _) (env-add! sym kind #f)])]
               [(id? sym) (env-add! sym 'value #f)]
               [(word? sym) (env-add! sym 'type (type:imported sym 'unknown))])))
     h]))

(define (tc-definition def)
  (with-handlers ([tcfail? (lambda (e)
                             #;(eprintf "FAILED: ~e\n" e)
                             (ambiguous (list def)))])
    (tc-definition* def)))

(define (tc-definition* def)
  (match def
    [(assign:word name params rhs)
     (match (ast-kind* rhs)
       ['type
        (define ast (tc-type (maybe-fun params rhs)))
        (tc-definition* (assign:type name ast))]
       ['class
        (define ast (maybe-fun params rhs))
        (tc-definition* (assign:class name ast))]
       [k (fail 'def k def)])]
    [(assign:id name params kind rhs)
     (match (ast-kind* kind)
       ['type
        (define ast ((tc-value kind) (maybe-fun params rhs)))
        (tc-definition* (assign:value name kind ast))]
       ['class
        (define ast ((tc-object kind) (maybe-fun params rhs)))
        (tc-definition* (assign:object name kind ast))]
       [k (fail 'def k def)])]
    [(assign:x-set name params kind rhs)
     (match (ast-kind* kind)
       ['type
        (define ast (tc-value-set kind (maybe-fun params rhs)))
        (tc-definition* (assign:value-set name kind ast))]
       ['class
        (define ast ((tc-object-set kind) (maybe-fun params rhs)))
        (tc-definition* (assign:object-set name kind ast))]
       [_ (fail 'def 'x-set def)])]

    [(assign:type name ast) (begin (env-add! name 'type ast) def)]
    [(assign:class name ast) (begin (env-add! name 'class ast) def)]
    [(assign:value name kind ast) (begin (env-add! name 'value ast) def)]
    [(assign:object name kind ast) (begin (env-add! name 'object ast) def)]
    [(assign:value-set name kind ast) (begin (env-add! name 'value-set ast) def)]
    [(assign:object-set name kind ast) (begin (env-add! name 'object-set ast) def)]

    [#f #f]))

(define (disambiguate kind tc t v)
  (match-define (ambiguous vs) v)
  (match (map-ok (tc t) vs)
    [(list v) v]
    [vs* (fail kind t (ambiguous vs*))]))

(define (tc-argument ast)
  (case (ast-kind* ast)
    [(value) ((tc-value #f) ast)]
    [(type) (tc-type ast)]
    [(value-set) (tc-value-set #f ast)]
    [(class) ((tc-object #f) ast)]
    [(object-set) ((tc-object-set #f) ast)]
    [else
     (match ast
       [(x-set:defn name) name] ;; FIXME: hack around bad parse?
       [_
        #;(eprintf "tc-argument: unknown kind: ~e\n" ast)
        ast])]))

(define (tc-type t)
  (match t
    [(? (ref-of-kinds? '(type)) ref) ref]
    [(ambiguous vs) (disambiguate 'type (lambda (_) tc-type) #f t)]
    [(expr:apply fun args)
     (let ([fun (tc-type fun)])
       ;; FIXME: use fun type information
       (expr:apply fun (map tc-argument args)))]
    [(expr:fun params body)
     (expr:fun params
               (parameterize ((env (env-add-params (env) params)))
                 (tc-type body)))]
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
     (expr:fun params
               (parameterize ((env (env-add-params (env) params)))
                 ((tc-value t) body)))]
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
  (ast:named name (check-thing value)))

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
  (ast:named name ((tc-value type) v)))

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

(define (typecheck mod [iters 10] #:loud? [loud? #f])
  (define (initial-pass defs)
    (for/list ([def (in-list defs)])
      (when loud?
        (when #f
          (eprintf "type checking:\n")
          (pretty-print def)
          (eprintf "=>\n")))
      (define ddef (tc-definition def))
      (when loud? (pretty-print ddef))
      ddef))
  (define (disambiguation-pass defs)
    (for/list ([def (in-list defs)])
      (match def
        [(ambiguous (list def))
         (when loud?
           (when #f
             (eprintf "disambiguating:\n")
             (pretty-print def)
             (eprintf "=>\n")))
         (define ddef (tc-definition def))
         (when loud?
           (cond [(ambiguous? ddef)
                  (eprintf "...still ambiguous\n")]
                 [else (pretty-print ddef)]))
         ddef]
        [def def])))
  (parameterize ((env base-env))
    (define h* (tc-header mod))
    (when loud? (pretty-print h*))
    (define defs*
      (for/fold ([defs (initial-pass (mod:defn-assignments mod))])
                ([i (in-range iters)] #:when (ormap ambiguous? defs))
        (when loud? (eprintf "-- DISAMBIGUATION PASS ~s --\n" i))
        (disambiguation-pass defs)))
    (define mod* (apply-tagging-mode (join-mod h* defs*)))
    (begin ;; update env with result of applying tag mode
      (for-each tc-definition (mod:defn-assignments mod*)))
    (module-env-add! mod* (env))
    mod*))

;; ============================================================

;; Tagging mode (p213, 12.1.2 Global tagging mode)
;; - IMPLICIT TAGS: applies to all components with *definite-tagged* types
;; - definite-tagged: primitives, EXPLICIT tagged, SET, SEQUENCE
;; - indefinite-tagged: CHOICE, ANY, parameter, "open type"
;;   - what about IMPLICIT tagged type?
;; - Note: must know openness of imported type (see p214 example)
;; - AUTOMATIC TAGS: components tagged sequentially from 0
;;   - disabled for any SET/SEQ/CHOICE that alreay has tag-annotated component

;; type-definite : Type -> (U 'definite 'indefinite 'unknown)
(define (type-definite t)
  (match (ast-eval t)
    [(type 'ANY) 'indefinite]
    [(type _) 'definite]
    [(? type:bit-string?) 'definite]
    [(? type:choice?) 'indefinite]
    [(? type:enum?) 'definite]
    [(? type:integer?) 'definite]
    [(? type:sequence?) 'definite]
    [(? type:set?) 'definite]
    [(? type:set-of?) 'definite]
    [(? type:sequence-of?) 'definite]
    [(type:tagged tag mode type) 'definite]
    [(type:constrained type _) (type-definite type)]
    [(type:any-defined-by id) 'indefinite]
    [(type:from-object object field) 'indefinite]
    [(type:from-class class field) 'indefinite]
    [(type:instance-of oid) 'indefinite]
    [(type:select id type) 'unknown] ;; FIXME
    [(type:param) 'indefinite]
    [(type:imported sym definite) definite]
    [_ 'unknown]))

(define (apply-tagging-mode ast)
  (define (handle-ast ast recur)
    (match ast
      [(mod:defn id tagmode extmode exports imports defs)
       (mod:defn id 'explicit extmode exports imports (do-tagging-mode tagmode defs))]
      [ast (recur ast)]))
  (tree-transform-preorder ast handle-ast))

(define (do-tagging-mode mode ast)
  (match mode
    [(or 'explicit 'implicit #f) (do-tagging-mode/indep (or mode 'explicit) ast)]
    ['automatic (do-tagging-mode/auto ast)]))

(define (do-tagging-mode/indep mode ast)
  (define (handle-type t)
    (match t
      ;; FIXME: also applies to type:tagged???
      [(type:sequence cs) (type:sequence (map handle-component cs))]
      [(type:set cs) (type:set (map handle-component cs))]
      [(type:choice alts) (type:choice (map handle-alternative alts))]
      [t t]))
  (define (handle-component-type t)
    (match t
      [(type:tagged tag #f t)
       (match mode
         ['explicit
          (type:tagged tag 'explicit t)]
         ['implicit
          (match (type-definite t)
            ['definite (type:tagged tag 'implicit t)]
            ['indefinite (type:tagged tag 'explicit t)]
            ['unknown (fail 'apply-tagging-mode mode t)])])]
      [t t]))
  (define (handle-component c)
    (match c
      [(opt:optional c) (opt:optional (handle-component c))]
      [(opt:default c d) (opt:default (handle-component c) d)]
      [(ast:named name t) (ast:named name (handle-component-type t))]))
  (define (handle-alternative alt)
    (match alt [(ast:named name t) (ast:named name (handle-component-type t))]))
  (tree-transform ast handle-type))

(define (do-tagging-mode/auto ast)
  (define (type:tagged/non-default? x)
    (and (type:tagged? x) (not (eq? (type:tagged-mode x) #f))))
  (define (handle-type t)
    (match t
      [(type:sequence cs) (type:sequence (handle-components cs))]
      [(type:set cs) (type:set (handle-components cs))]
      [(type:choice alts) (type:choice (handle-alternatives alts))]
      [t t]))
  (define (handle-components cs)
    (cond [(for/or ([c (in-list cs)])
             (type:tagged/non-default?
              (match c
               [(ast:named _ t) t]
               [(opt:optional (ast:named _ t)) t]
               [(opt:default (ast:named _ t) _) t])))
           cs]
          [else
           (for/list ([c (in-list cs)] [tagnum (in-naturals)])
             (handle-component1 c tagnum))]))
  (define (handle-alternatives alts)
    (cond [(for/or ([a (in-list alts)])
             (match a [(ast:named _ t) (type:tagged/non-default? t)]))
           alts]
          [else
           (for/list ([a (in-list alts)] [tagnum (in-naturals)])
             (handle-component1 a tagnum))]))
  (define (handle-component1 c tagnum)
    (match c
      [(opt:optional c) (opt:optional (handle-component1 c tagnum))]
      [(opt:default c d) (opt:default (handle-component1 c tagnum) d)]
      [(ast:named name t) (ast:named name (handle-component-type t tagnum))]))
  (define (handle-component-type t tagnum)
    (match (type-definite t)
      ['definite (type:tagged (tag 'context-sensitive tagnum) 'implicit t)]
      ['indefinite (type:tagged (tag 'context-sensitive tagnum) 'explicit t)]
      ['unknown (fail 'apply-tagging-mode 'automatic t)]))
  (tree-transform ast handle-type))

;; ============================================================

(module+ main
  (require racket/cmdline
           "ggramar2.rkt")

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
         (define mod (read-module in))
         (define defs (mod:defn-assignments (typecheck mod #:loud? #t)))
         (when #t (for-each check-ambiguous defs))
         (when #f
           (eprintf "\nApplied tagging mode (~s):\n" (mod:defn-tagmode mod))
           (define tmod (apply-tagging-mode mod))
           (pretty-print tmod)
           (newline))
         (let ([amb (count ambiguous? defs)])
           (cond [(zero? amb) (eprintf "Success.\n")]
                 [else (eprintf "Failed to eliminate ~s ambiguities.\n" amb)])))))))
