;; Copyright 2017-2019 Ryan Culpepper
;; 
;; This library is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library.  If not, see <http://www.gnu.org/licenses/>.

#lang racket/base
(require racket/contract/base
         racket/match
         (for-syntax racket/base syntax/transformer)
         "private/base.rkt"
         "private/types.rkt"
         "private/ber-frame.rkt"
         "private/ber.rkt")
(provide (rename-out [BER-frame* BER-frame])
         BER-frame?
         BER-frame-tag-class
         BER-frame-tag-number
         BER-frame-content
         (contract-out
          [read-BER-frame
           (->* [] [input-port? #:der? any/c #:limit (or/c exact-nonnegative-integer? #f)]
                BER-frame?)]
          [write-BER-frame
           (->* [BER-frame?] [output-port? #:der? any/c] void?)]
          [BER-encode (->* [asn1-type? any/c] [#:der? any/c] BER-frame?)]
          [BER-decode (->* [asn1-type? BER-frame?] [#:der? any/c] any/c)]))

(define (make-BER-frame tagclass tagn content)
  (BER-frame (make-tag tagclass tagn) content))

(define-module-boundary-contract make-BER-frame* make-BER-frame
  (-> (or/c 'universal 'application 'context-sensitive 'private)
      exact-positive-integer?
      (or/c bytes? (listof (or/c bytes? BER-frame?)))
      BER-frame?)
  #:name-for-blame BER-frame)

(define-match-expander BER-frame*
  (syntax-rules ()
    [(_ tagclass tagn content)
     (? BER-frame?
        (app BER-frame-tag-class tagclass)
        (app BER-frame-tag-number tagn)
        (app BER-frame-content content))])
  (make-variable-like-transformer #'make-BER-frame*))

(define (read-BER-frame [in (current-input-port)] #:der? [der? #f] #:limit [limit #f])
  (define br (make-asn1-binary-reader in #:limit limit))
  (read-frame br der?))

;; BER-decode : Type BER-Frame Boolean -> Any
(define (BER-decode type frame #:der? [der? #f])
  (decode-frame type frame der?))
