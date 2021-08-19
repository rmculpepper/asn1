#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/parse racket/promise
                     racket/match racket/dict syntax/id-table racket/list)
         grrparse)
(provide (all-defined-out))

(define-syntax-rule (expression/begin-for-syntax e)
  (#%expression (let-syntaxes ([() (begin0 (#%plain-app values) e)]) (#%plain-app void))))

;; ------------------------------------------------------------

(define-syntax (define-nt-definers stx)
  (syntax-parse stx
    [(_ define-nt define-g)
     #'(begin
         (define-for-syntax ntbox (box null))
         (define-syntax define-nt (define-nt-tx ntbox))
         (define-syntax define-g (define-g-tx ntbox)))]))

(begin-for-syntax
  (define ((define-nt-tx ntbox) stx)
    (syntax-parse stx
      [(_ nt:id prod ...)
       #:with ntdef #'(nt prod ...)
       (set-box! ntbox (cons (syntax-local-introduce #'ntdef) (unbox ntbox)))
       #'(void)]))
  (define ((define-g-tx ntbox) stx)
    (syntax-parse stx
      [(_ name:id)
       (with-syntax ([(ntdef ...)
                      (map syntax-local-introduce (reverse (unbox ntbox)))])
         #'(define-grammar name ntdef ...))])))
