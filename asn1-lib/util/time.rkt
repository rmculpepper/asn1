;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract
         racket/match
         racket/date
         "../main.rkt"
         (submod "../main.rkt" private-util-time))
(provide (contract-out
          [asn1-utc-time->seconds
           (-> asn1-utc-time? any)]
          [asn1-generalized-time->seconds
           (-> asn1-generalized-time? any)]))

;; asn1-utc-time->seconds : String -> Real
;; Interprets YY in the range [1950,2049], following X.500 WG and others.
(define (asn1-utc-time->seconds t
                                #:who [who 'asn1-utc-time->seconds])
  (match (regexp-match UTCTime-rx t)
    [(list _ YY (regexp XXYY-rx (list _ MM DD)) hh mm ss offset)
     (to-seconds who #f YY MM DD hh mm ss #f offset)]
    [#f (error who "invalid UTCTime\n  string: ~e" t)]))

;; asn1-generalized-time->seconds : String -> Real
(define (asn1-generalized-time->seconds t
                                        #:who [who 'asn1-generalized-time->seconds])
  (match (regexp-match GeneralizedTime-rx t)
    [(list _ YYYY (regexp XXYY-rx (list _ MM DD)) hh mm ss ff offset)
     (to-seconds who YYYY #f MM DD hh mm ss ff offset)]
    [#f (error who "invalid GeneralizedTime\n  string: ~e" t)]))

(define (to-seconds who YYYY YY MM DD hh mm ss ff offset)
  (define ($ s) (if s (string->number s) 0))
  (let ([YYYY (cond [YYYY ($ YYYY)]
                    [else (let ([YY ($ YY)]) (+ YY (if (< YY 50) 2000 1900)))])]
        [MM ($ MM)] [DD ($ DD)] [hh ($ hh)] [mm ($ mm)] [ss ($ ss)]
        [ff (if ff (string->number (string-append "0." ff)) 0)]
        [offset (offset->seconds offset)])
    (+ (find-seconds ss mm hh DD MM YYYY (not offset)) (or offset 0) ff)))

(define XXYY-rx #px"^([0-9]{2})([0-9]{2})$")

(define (offset->seconds offset)
  (define ($ s) (if s (string->number s) 0))
  (match offset
    [#f #f]
    ["Z" 0]
    [(regexp #rx"^([+-]?)([01][0-9]|2[0-3])([0-5][0-9])?$"
             (list _ sign hh mm))
     ;; The offset represents an offset already added to UTC time,
     ;; so invert the sign.
     (* (if (equal? sign "-") 1 -1) 60 (+ ($ mm) (* 24 ($ hh))))]))
