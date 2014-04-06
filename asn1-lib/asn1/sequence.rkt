#lang racket/base
(require racket/contract
         racket/match)
(provide
 (contract-out
  [sequence?
   (-> any/c boolean?)]
  [sequence-ref
   (->* [sequence? symbol?] [any/c] any)]
  [list->sequence
   (->* [any/c (listof symbol?)] [(listof symbol?)] any)]
  [sequence->list
   (->* [any/c (listof symbol?)] [(listof symbol?)] any)]))

;; ============================================================

(define (sequence? x)
  (match x
    [(cons 'sequence (list (list (? symbol?) _) ...))
     #t]
    [_ #f]))

(define none (gensym 'none))

(define (sequence-ref s k [default none])
  (cond [(assq k (cdr s)) => cadr]
        [(eq? default none)
         (error 'sequence-ref
                "key not found in sequence\n  key: ~e\n  sequence: ~e"
                k s)]
        [(procedure? default) (default)]
        [else default]))

;; list->sequence and sequence->list
;; List must contain all mandatory fields and a *prefix* of optional fields.
(define (list->sequence v0 mand-fields [opt-fields null])
  (define mand-field-count (length mand-fields))
  (define (loop fields v index)
    (cond [(and (pair? fields) (pair? v))
           (cons (list (car fields) (car v))
                 (loop (cdr fields) (cdr v) (add1 index)))]
          [(pair? fields)
           (if (< index mand-field-count) ;; mandatory
               (error 'list->sequence
                      "missing mandatory field\n  field: ~e\n  value: ~e"
                      (car fields) v0)
               (loop (cdr fields) v (add1 index)))]
          [(pair? v)
           (error 'list->sequence
                  "too many values in list\n  extra values: ~e"
                  (car v))]
          [else null]))
  (cons 'sequence (loop (append mand-fields opt-fields) v0 0)))

(define (sequence->list v mand-fields [opt-fields null])
  (define mand-field-count (length mand-fields))
  (define (loop lvs fields index first-missing-field)
    (cond [(pair? fields)
           (cond [(assq (car fields) lvs)
                  => (lambda (lv)
                       (when first-missing-field
                         (error 'sequence->list
                                (string-append "present field after missing optional field"
                                               "\n  missing: ~e\n  present: ~e\n  value: ~e")
                                first-missing-field (car fields) v))
                       (cons (cadr lv)
                             (loop lvs (cdr fields) (add1 index) #f)))]
                 [(< index mand-field-count) ;; mandatory
                  (error 'sequence->list
                         "missing mandatory field\n  field: ~e\n  value: ~e"
                         (car fields) v)]
                 [else
                  (loop lvs (cdr fields) (add1 index) (car fields))])]
          [else null]))
  (match v
    [(cons 'sequence (and lvs (list (list (? symbol?) _) ...)))
     (loop lvs (append mand-fields opt-fields) 0 #f)]
    [_ (error 'sequence->list "ill-formed sequence value\n  value: ~e" v)]))
