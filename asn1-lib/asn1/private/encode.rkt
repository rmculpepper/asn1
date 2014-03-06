;; Copyright 2014 Ryan Culpepper
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
(require racket/match
         racket/promise
         "base256.rkt"
         "base-types.rkt"
         "types.rkt")
(provide (all-defined-out))

;; DER encoding and decoding

;; ============================================================

;; Encode/Decode hooks

;; DER-encode-hooks : (parameterof (listof (cons Asn1-Type DER-Encode-Hook)))
;; A DER-Encode-Hook is (list 'pre (Asn1-Type Any -> Bytes)), provides value bytes
(define DER-encode-hooks (make-parameter null))

;; DER-decode-hooks : (parameterof (listof (cons Asn1-Type DER-Decode-Hook)))
;; A DER-Decode-Hook is one of
;; - (list 'pre  (Asn1-Type Bytes -> Any))
;; - (list 'post (Asn1-Type Any   -> Any))
;; Note: a pre-hook prevents a post-hook from running.
(define DER-decode-hooks (make-parameter null))

;; search-hooks : Symbol (listof Key) (listof (list Key Symbol Value))
;;             -> (list Key Symbol Value) or #f
;; Search back to front.
(define (search-hooks kind keys hooks)
  (let loop ([keys keys])
    (and (pair? keys)
         (or (loop (cdr keys))
             (get-hook kind (car keys) hooks)))))

;; get-hook : Symbol Key (listof (list Key Symbol Value)) -> (list Key Symbol Value) or #f
(define (get-hook kind key hooks)
  (for/or ([hook (in-list hooks)])
    (and (eq? (car hook) key)
         (eq? (cadr hook) kind)
         hook)))

;; ============================================================

;; DER-encode : T V[T] -> E[T]
;; - bytes/integer/etc    Base-Type                          -> E[_]
;; - (SequenceOf T)       (list V[T] ...)                    -> E[_]
;; - (Sequence [L T] ...) (list 'sequence (list L V[T]) ...) -> E[_]
;; - (Sequence [L T] ...) (list V[T] ...)                    -> E[_]
;; - (SetOf T)            (list V[T] ...)                    -> E[_]
;; - (Set [L T] ...)      (list 'set (list L V[T]) ...)      -> E[_]
;; - (Choice [L T] ...)   (list L V[T])                      -> E[_]

(define (DER-encode type v [alt-tag #f])
  (let loop ([type type] [alt-types null])
    (match type
      [(asn1-type:any)
       ;; Note: no wrapping; encoder must produce whole TLV triple
       ;; alt-tag must be #f; can't implicitly tag an ANY value
       (DER-encode-value type v alt-types)]
      [(asn1-type:base base-type)
       (wrap base-type (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:sequence _)
       (wrap 'SEQUENCE (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:sequence-of _)
       (wrap 'SEQUENCE (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:set _)
       (wrap 'SET (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:set-of _)
       (wrap 'SET (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:choice elts)
       (match v
         [(list (? symbol? sym) v*)
          (match-define (element-type _ tag* type* _)
            (for/or ([elt (in-list elts)])
              (and (eq? (element-type-name elt) sym) elt)))
          (DER-encode type* v* tag*)]
         [_ (error 'asn1-encode "bad value for Choice type\n  value: ~e" v)])]
      [(asn1-type:explicit-tag _)
       (wrap 'SEQUENCE (DER-encode-value type v alt-types) alt-tag)]
      [(asn1-type:defined name promise)
       (loop (force promise) (cons type alt-types))])))

(define (DER-encode-value type v [alt-types null])
  ;; Search alt-types back-to-front, then type, for hook to apply
  (define hook
    (let ([hooks (DER-encode-hooks)])
      (or (search-hooks 'pre alt-types hooks)
          (get-hook 'pre type hooks))))
  (if hook
      (let ([hook-f (caddr hook)])
        (let ([b (hook-f type v)])
          (unless (bytes? b)
            (error 'DER-encode-value
                   "value returned by encode-hook is not bytes\n  value: ~e"
                   b))
          b))
      (DER-encode-value* type v)))

(define (DER-encode-value* type v)
  (match type
    [(asn1-type:any)
     ;; To make ANY work, need to use encode hook
     (error 'DER-encode-value
            "no default encoding rule for ANY\n  value: ~e" v)]
    [(asn1-type:base base-type)
     (DER-encode-base* base-type v)]
    [(asn1-type:sequence elts)
     (encode-sequence (filter values (DER-encode-sequence* elts v)))]
    [(asn1-type:sequence-of type*)
     (unless (list? v)
       (error 'DER-encode-value "bad value for SequenceOf type\n  value: ~e" v))
     (encode-sequence
      (for/list ([v* (in-list v)])
        (DER-encode type* v* #f)))]
    [(asn1-type:set elts)
     (encode-set (filter values (DER-encode-set* elts v)))]
    [(asn1-type:set-of type*)
     (unless (list? v)
       (error 'DER-encode-value "bad value for SetOf type\n  value: ~e" v))
     (encode-set
      (for/list ([v* (in-list v)])
        (DER-encode type* v* #f)))]
    [(asn1-type:explicit-tag type*)
     (encode-sequence (list (DER-encode type* v)))]
    [(asn1-type:choice _)
     (error 'DER-encode-value "internal error: bad type\n  type: ~e" type)]
    [(asn1-type:defined _ _)
     (error 'DER-encode-value "internal error: bad type\n  type: ~e" type)]))

(define (DER-encode-base* base-type v)
  (define (bad-value [expected #f])
    (error 'DER-encode-value
           "bad value for type\n  type: ~s\n  value: ~e~a"
           base-type v
           (if expected (format "\n  expected: ~a" expected) "")))
  (case base-type
    [(INTEGER)
     (unless (exact-integer? v) (bad-value 'exact-integer?))
     (signed->base256 v)]
    [(BIT-STRING)
     (unless (bytes? v) (bad-value 'bytes?))
     (encode-bit-string v)]
    [(OCTET-STRING)
     (unless (bytes? v) (bad-value 'bytes?))
     v]
    [(NULL)
     (unless (eq? v #f) (bad-value "#f"))
     #""]
    [(OBJECT-IDENTIFIER)
     (unless (and (list? v) (andmap exact-nonnegative-integer? v))
       (bad-value '(listof exact-nonnegative-integer?)))
     (encode-object-identifier v)]
    ;; Sequence[Of], Set[Of]
    [(PrintableString)
     (unless (printable-string? v) (bad-value 'printable-string?))
     (string->bytes/latin-1 v)]
    ;; T61String
    [(IA5String)
     (unless (ia5string? v) (bad-value 'ia5string?))
     (string->bytes/latin-1 v)]
    ;; UTCTime
    [else (error 'DER-encode-value "unsupported base type\n  type: ~s" base-type)]))

;; DER-encode-sequence* : (listof ElementType) Any -> (listof (U Bytes #f))
(define (DER-encode-sequence* elts v)
  (match v
    [(cons 'sequence lvs)
     (match lvs
       [(list (list (? symbol?) _) ...)
        (let loop ([elts elts] [lvs lvs])
          (cond [(and (null? elts) (null? lvs))
                 null]
                [(null? elts)
                 (error 'DER-encode-value
                        "unexpected field in Sequence value\n  value: ~e\n  field: ~s"
                        v (car (car lvs)))]
                [else
                 (match (car elts)
                   [(element-type name tag* type* option)
                    (cond [(and (pair? lvs)
                                (eq? (car (car lvs)) name))
                           (cons (DER-encode type* (cadr (car lvs)) tag*)
                                 (loop (cdr elts) (cdr lvs)))]
                          [option
                           (loop (cdr elts) lvs)]
                          [else
                           (error 'DER-encode-value
                                  "missing field in Sequence value\n  value: ~e\n  field: ~s~a"
                                  name v
                                  (if (pair? lvs)
                                      (format "\n  got: ~s" (car (car lvs)))
                                      ""))])])]))]
       [_ (error 'DER-encode-value "bad value for Sequence\n  value: ~e" v)])]
    [(list _ ...)
     (unless (= (length v) (length elts))
       (error 'DER-encode-value "wrong number of elements for Sequence\n  value: ~e" v))
     (for/list ([v* (in-list v)]
                [elt (in-list elts)])
       (match elt
         [(element-type name tag* type* option)
          (DER-encode type* v* tag*)]))]
    [_
     (error 'DER-encode-value "bad value for Sequence\n  value: ~e" v)]))

;; DER-encode-set* : (listof ElementType) Any -> (listof (U Bytes #f))
(define (DER-encode-set* elts v)
  (define lvs
    (match v
      [(list 'set (and lvs (list (list l v) ...))) lvs]
      [_ (error 'DER-encode-value "bad value for Set type\n  value: ~e" v)]))
  (for/list ([elt (in-list elts)])
    (match elt
      [(element-type name tag* type* option)
       (define default
         (match option [(list 'default default) default] [_ #f]))
       (cond [(assq name lvs)
              => (lambda (lv)
                   (define v* (cadr lv))
                   (if (equal? v* default)
                       #f
                       (DER-encode type* v* tag*)))]
             [(equal? option '(optional))
              #f]
             [default
               ;; Don't encode default
               #f]
             [else
              (error 'DER-encode-value "no value for Set field\n  field: ~s\n  value: ~e"
                     name v)])])))

;; ----

;; Base type encoders

;; encode-boolean : boolean -> bytes
(define (encode-boolean b)
  (if b #"\1" #"\0"))

;; encode-bit-string : bytes nat -> bytes
(define (encode-bit-string bits trailing-unused)
  (cond [(zero? (bytes-length bits))
         (unless (zero? trailing-unused)
           (error 'encode-bit-string
                  "trailing unused bits non-zero for empty bit string\n  value: ~e\n  trailing unused bits: ~s"
                  bits trailing-unused))]
        [else
         (unless (zero? (bitwise-bit-field (bytes-ref bits (sub1 (bytes-length bits))) 0 trailing-unused))
           (error 'encode-bit-string "trailing unused bits are not 0\n  value: ~e\n  trailing unused bits: ~s"
                  bits trailing-unused))])
  (bytes-append (bytes trailing-unused) bits))

;; encode-ia5string : String -> Bytes
(define (encode-ia5string s)
  (unless (ia5string? s)
    (raise-argument-error 'encode-ia5string "ia5string?" s))
  (string->bytes/latin-1 s))

;; encode-integer : Exact-Integer -> Bytes
(define (encode-integer n)
  (unless (exact-integer? n)
    (error 'encode-integer "not an exact integer: ~e" n))
  (signed->base256 n))

;; encode-null : Any -> Bytes
(define (encode-null [_ignored #f])
  #"")

;; encode-object-identifier : (listof (U Nat (List Symbol Nat))) -> Bytes
(define (encode-object-identifier cs)
  (let ([cs (for/list ([c (in-list cs)])
              (if (list? c) (cadr c) c))])
    (let ([c1 (car cs)]
          [c2 (cadr cs)]
          [cs* (cddr cs)])
      (apply bytes-append
             (bytes (+ (* 40 c1) c2))
             (map encode-oid-component cs*)))))
(define (encode-oid-component c)
  (define (loop c acc)
    (if (zero? c)
        acc
        (let-values ([(q r) (quotient/remainder c 128)])
          (loop q (cons (bitwise-ior 128 r) acc)))))
  (apply bytes
         (let-values ([(q r) (quotient/remainder c 128)])
           (loop q (list r)))))

;; encode-octet-string : Bytes -> Bytes
(define (encode-octet-string b)
  b)

;; encode-printable-string : Printable-String -> Bytes
(define (encode-printable-string s)
  (string->bytes/latin-1 s))

(define (encode-utf8string s)
  (string->bytes/utf-8 s))

;; encode-sequence : (listof Bytes) -> Bytes
(define (encode-sequence lst)
  (apply bytes-append lst))

;; encode-set : (listof Bytes) -> Bytes
(define (encode-set lst)
  (apply bytes-append (sort lst bytes<?)))

;; ============================================================

(define (DER-read type in)
  (DER-decode-frame type (read-frame in)))

(define (DER-decode type b)
  (DER-decode-frame type (bytes->frame b)))

(define (DER-decode-frame type frame)
  (match-define (der-frame tagclass p/c tagn c) frame)
  (let loop ([type type] [alt-types null] [check-whole-tag? #t])
    ;; check-type : Base-Type -> Void
    (define (check-type base-type)
      (define te (type->tag-entry base-type))
      (unless te (error 'DER-decode "unknown base type\n  type: ~s" base-type))
      (when check-whole-tag?
        (unless (equal? tagclass 'universal)
          (error 'DER-decode "tag class mismatch\n  expected: ~s\n  decoded: ~s"
                 'universal tagclass))
        (unless (equal? tagn (tag-entry-tagn te))
          (error 'DER-decode "tag number mismatch\n  expected: ~s\n  decoded: ~s"
                 (tag-entry-tagn te) tagn)))
      (unless (equal? p/c (tag-entry-p/c te))
        (error 'DER-decode "primitive vs constructed mismatch\n  expected: ~s\n  decoded: ~s"
               (tag-entry-p/c te) p/c)))

    (define (decode-value)
      (DER-decode-value type c alt-types))

    (match type
      [(asn1-type:any)
       (DER-decode-value type (frame->bytes frame))]
      [(asn1-type:base base-type)
       (check-type base-type)
       (decode-value)]
      [(asn1-type:sequence _)
       (check-type 'SEQUENCE)
       (decode-value)]
      [(asn1-type:sequence-of _)
       (check-type 'SEQUENCE)
       (decode-value)]
      [(asn1-type:set _)
       (check-type 'SET)
       (decode-value)]
      [(asn1-type:set-of type*)
       (check-type 'SET)
       (decode-value)]
      [(asn1-type:choice elts)
       (let choice-loop ([elts elts])
         (match elts
           [(cons (and elt0 (element-type _ _ et-type _)) rest-elts)
            (if (tag-matches elt0 frame)
                (loop et-type (cons type alt-types) #f)
                (choice-loop rest-elts))]
           [_ (error 'DER-decode "tag does not match any alternative in Choice")]))]
      [(asn1-type:explicit-tag type*)
       ;; Tag has already been checked by enclosing CHOICE, SEQUENCE, or SET
       (unless (equal? p/c 'constructed)
         (error 'DER-decode "primitive vs constructed mismatch\n  expected: ~s\n  decoded: ~s"
                'constructed p/c))
       (decode-value)]
      [(asn1-type:defined name promise)
       (loop (force promise) (cons type alt-types) check-whole-tag?)])))

;; tag-matches : Element-Type DER-Frame -> Boolean
;; Checks class and tag number for match; FIXME: check p/c
(define (tag-matches elt frame)
  ;; (match-define (element-type _ et-tag et-type _) elt)
  (match-define (der-frame f-tagclass f-p/c f-tagn _) frame)
  (define et-tags (type->tags elt))
  (for/or ([et-tag (in-list et-tags)])
    ;; FIXME: need to consider p/c !!!
    (or (eq? et-tag #f) ;; #f=ANY matches all tags
        (and (equal? f-tagclass (car et-tag))
             (equal? f-tagn (cadr et-tag))))))

;; DER-decode-value : Asn1-Type Bytes -> Any
;; Note: if type is ANY, c is whole TLV triple; otherwise, just value part.
(define (DER-decode-value type c [alt-types null])
  (define hooks (DER-decode-hooks))
  (define pre-hook
    (or (search-hooks 'pre alt-types hooks)
        (get-hook 'pre type hooks)))
  (if pre-hook
      (let ([pre-hook-f (caddr pre-hook)])
        (pre-hook-f type c))
      (let* ([post-hook
              (or (search-hooks 'post alt-types hooks)
                  (get-hook 'post type hooks))]
             [v (DER-decode-value* type c)])
        (if post-hook
            (let ([post-hook-f (caddr post-hook)])
              (post-hook-f type v))
            v))))

(define (DER-decode-value* type c)
  (match type
    [(asn1-type:any)
     (error 'DER-decode-value
            "no default decoding rule for ANY")]
    [(asn1-type:base base-type)
     (DER-decode-base* base-type c)]
    [(asn1-type:sequence elts)
     (DER-decode-sequence* elts (bytes->frames c))]
    [(asn1-type:sequence-of type*)
     (for/list ([frame (in-list (bytes->frames c))])
       (DER-decode-frame type* frame))]
    [(asn1-type:set elts)
     (DER-encode-set* elts c)]
    [(asn1-type:set-of type*)
     (for/list ([frame (in-list (bytes->frames c))])
       (DER-decode-frame type* frame))]
    [(asn1-type:explicit-tag type*)
     (DER-decode type* (bytes->frames c))]
    [(asn1-type:choice elts)
     (error 'DER-decode-value "internal error: bad type\n  type: ~e" type)]
    [(asn1-type:defined name promise)
     (error 'DER-decode-value "internal error: bad type\n  type: ~e" type)]))

(define (DER-decode-base* base-type c)
  (define (bad-value [expected #f])
    (error 'DER-decode-value
           "bad value for type\n  type: ~s\n  value: ~e~a"
           base-type c
           (if expected (format "\n  expected: ~a" expected) "")))
  (case base-type
    [(INTEGER)
     (base256->signed c)]
    [(BIT-STRING)
     (decode-bit-string c)]
    [(OCTET-STRING)
     c]
    [(NULL)
     #f]
    [(OBJECT-IDENTIFIER)
     (decode-object-identifier c)]
    ;; Sequence[Of], Set[Of]
    [(PrintableString)
     (decode-printable-string c)]
    ;; T61String
    [(IA5String)
     (decode-ia5string c)]
    ;; UTCTime
    [else (error 'DER-decode-value "unsupported base type\n  type: ~s" base-type)]))

;; DER-decode-sequence* : (listof ElementType) (listof Frame)
;;                     -> (cons 'sequence (listof (list Symbol Any)))
(define (DER-decode-sequence* elts frames)
  (cons 'sequence
    (let loop ([elts elts] [frames frames])
      (match elts
        [(cons (and elt0 (element-type et-name et-tag et-type et-option)) rest-elts)

         ;; current element is missing; try to skip
         (define (try-skip rest-frames)
           (match et-option
             ['(optional)
              (loop rest-elts rest-frames)]
             [(list 'default default-value)
              (cons (list et-name default-value)
                    (loop rest-elts rest-frames))]
             [#f
              (error 'DER-decode-value
                     "missing field in encoded Sequence\n  field: ~s"
                     et-name)]))

         (match frames
           [(cons (and frame0 (der-frame f-tagclass f-p/c f-tagn f-c)) rest-frames)
            (cond [(tag-matches elt0 frame0)
                   (cons (list et-name (DER-decode-frame et-type frame0))
                         (loop rest-elts rest-frames))]
                  [else (try-skip rest-frames)])]
           ['()
            (try-skip '())])]
        ['()
         (if (null? frames)
             null
             (error 'DER-decode-value
                    "leftover components in encoded Sequence"))]))))

;; DER-decode-set* : (listof ElementType) (listof Frame)
;;                -> (cons 'set (listof (list Symbol Any)))
(define (DER-decode-set* elts frames)
  (cons 'set
    (let loop ([elts elts] [frames frames])
      (match elts
        [(cons (and elt0 (element-type et-name _ et-type et-option)) rest-elts)
         (cond [(for/first ([frame (in-list frames)]
                            #:when (tag-matches elt0 frame))
                  frame)
                => (lambda (frame0)
                     (cons (list et-name (DER-decode-frame et-type frame0))
                           (loop rest-elts (remq frame0 frames))))]
               [else
                ;; current element is missing; try to skip
                (match et-option
                  ['(optional)
                   (loop rest-elts frames)]
                  [(list 'default default-value)
                   (cons (list et-name default-value)
                         (loop rest-elts frames))]
                  [#f
                   (error 'DER-decode-value
                          "missing field in encoded Set\n  field: ~s" et-name)])])]
        ['()
         (if (null? frames)
             null
             (error 'DER-decode-value
                    "leftover components in encoded Set"))]))))

;; ----

;; Base type decoders

;; decode-boolean : Bytes -> Boolean
(define (decode-boolean b)
  (cond [(equal? b #"\1") #t]
        [(equal? b #"\0") #f]
        [else (error 'decode-boolean "bad BOOLEAN encoding\n  encoding: ~e" b)]))

;; decode-bit-string : bytes -> bytes
;; Given encoded content, returns raw bit string
;; FIXME: trailing-unused bits must be zero!
(define (decode-bit-string c)
  (when (zero? (bytes-length c))
    (error 'decode-bit-string "bad encoding for BIT STRING: empty"))
  (let ([trailing-unused (bytes-ref c 0)])
    (unless (zero? trailing-unused)
      ;; FIXME: support ... but with what representation?
      (error 'decode-bit-string "BIT STRING with partial octets not supported"))
    (subbytes c 1 (bytes-length c))))

;; decode-ia5string : Bytes -> String
(define (decode-ia5string bs)
  (define s (bytes->string/latin-1 bs))
  (unless (ia5string? s)
    (error 'decode-ia5string "not an ia5string: ~e" s))
  s)

;; decode-integer : bytes -> integer
;; Given encoded integer, returns raw integer
(define (decode-integer bs)
  (base256->signed bs))

;; decode-null : bytes -> #f
(define (decode-null bs)
  (unless (equal? bs #"")
    (error 'decode-null "bad encoding of NULL\n  encoding: ~e" bs))
  #f)

;; decode-object-identifier : Bytes -> (listof Nat)
(define (decode-object-identifier bs)
  (when (zero? (bytes-length bs))
    (error 'decode-object-identifier "empty" bs))
  (define in (open-input-bytes bs))
  (define b1 (read-byte in))
  (list* (quotient b1 40) (remainder b1 40)
         (let loop ()
           (if (eof-object? (peek-byte in))
               null
               (let ([c (decode-oid-component in)])
                 (cons c (loop)))))))
(define (decode-oid-component in)
  (let loop ([c 0])
    (let ([next (read-byte in)])
      (cond [(eof-object? next)
             (error 'decode-object-identifier "incomplete component")]
            [(< next 128)
             (+ next (arithmetic-shift c 7))]
            [else
             (loop (+ (- next 128) (arithmetic-shift c 7)))]))))

;; decode-octet-string : Bytes -> Bytes
(define (decode-octet-string b)
  b)

;; decode-printable-string : Bytes -> Printable-String
(define (decode-printable-string bs)
  (let ([s (bytes->string/latin-1 bs)])
    (if (printable-string? s)
        s
        (error 'decode-printable-string "not a printable string: ~e" s))))

(define (decode-utf8string b)
  (if (bytes-utf-8-length b #f)
      (bytes->string/utf-8 b)
      (error 'decode-utf8string "not a UTF-8 string")))

;; ============================================================
;; ============================================================

;; BER (restricted to definite-length) encoding has 3:
;; - identifier - class, tag, primitive vs constructed
;; - length - number of contents octets
;; - content

;; == Primitive, definite-length ==
;; Tags:
;;   for low tag number (0-30):
;;     bits 8,7 are class, bit 6 is 0, bits 5-1 are tag number
;;     classes: universal (0,0); application (0,1); context-sensitive (1,0); private (1,1)
;;   for high tag number: ... (don't need yet)
;; Length octets:
;;   short (0 to 127): bit 8 is 0, bits 7-1 are value
;;   long (128 to 2^1008-1):
;;     first octet: bit 8 is 1, bits 7-1 give number of following length octets
;;     rest of octets are base-256 number

;; == Constructed, definite-length ==
;; Tags: same as primitive, except bit 6 is 1 to indicate constructed
;; Length octets: same as primitive

;; == Alternative tagging ==
;; class is context-sensitive unless overridden in defn
;; Implicit:
;;   change tag of component
;;   inherit prim/cons from underlying type
;; Explicit:
;;   adds outer tag
;;   always constructed

;; wrap : symbol bytes [Tag] -> bytes
(define (wrap type c [alt-tag #f])
  (define tag-entry
    (or (type->tag-entry type)
        (error 'wrap "unknown type: ~e" type)))
  (bytes-append (tag-entry->tag-bytes tag-entry alt-tag) (length-code c) c))

;; tag-entry->tag-bytes : TagEntry [Tag] -> bytes
(define (tag-entry->tag-bytes te [alt-tag #f])
  (if alt-tag
      (get-tag-bytes (car alt-tag) (tag-entry-p/c te) (cadr alt-tag))
      (get-tag-bytes 'universal (tag-entry-p/c te) (tag-entry-tagn te))))

;; get-tag-bytes : ... -> bytes
(define (get-tag-bytes class p/c tagn)
  (bytes
   (+ (case class
        [(universal)        0]
        [(application)      #b01000000]
        [(context-specific) #b10000000]
        [(private)          #b11000000])
      (case p/c
        [(primitive)   0]
        [(constructed) #b00100000])
      tagn)))

;; length-code : (U nat bytes) -> bytes
(define (length-code n)
  (if (bytes? n)
      (length-code (bytes-length n))
      (cond [(<= 0 n 127)
             (bytes n)]
            [else
             (let ([nc (unsigned->base256 n)])
               (unless (< 128 (bytes-length nc))
                 (error 'length-code "length too long: ~e" n))
               (bytes-append
                (bytes (bitwise-ior 128 (bytes-length nc)))
                nc))])))

;; ============================================================

;; FIXME: add checking for premature EOF, etc

;; A DER-Frame is (der-frame TagClass P/C TagNum bytes)
(struct der-frame (tagclass p/c tagn content) #:transparent)

;; bytes->frame : bytes -> DER-Frame
(define (bytes->frame der)
  (define in (open-input-bytes der))
  (begin0 (read-frame in)
    (unless (eof-object? (peek-char in))
      (error 'bytes->frame "bytes left over after DER TLV triple"))))

;; frame->bytes : DER-Frame -> Bytes
(define (frame->bytes frame)
  (match frame
    [(der-frame tagclass p/c tagn content)
     (bytes-append (get-tag-bytes tagclass p/c tagn)
                   (length-code content)
                   content)]))

;; bytes->frames : bytes -> (listof DER-Frame)
(define (bytes->frames der)
  (read-frames (open-input-bytes der)))

;; read-frame : input-port -> DER-Frame
(define (read-frame in)
  (let* ([tag (read-tag in)]
         [len (read-length-code in)]
         [c (read-bytes len in)])
    (der-frame (car tag) (cadr tag) (caddr tag) c)))

;; read-frames : input-port -> (listof DER-Frame)
(define (read-frames in)
  (if (eof-object? (peek-char in))
      null
      (cons (read-frame in) (read-frames in))))

;; read-tag : input-port -> (list TagClass P/C TagNum)
(define (read-tag in)
  (let* ([tag (read-byte in)]
         [c? (bitwise-bit-set? tag (sub1 6))]
         [tagclass (bitwise-bit-field tag 6 8)]
         [tagnum (bitwise-and tag 31)])
    (unless (<= 0 tagnum 30)
      (error 'bytes->frame "high tags not implemented"))
    (list (case tagclass
            [(#b00) 'universal]
            [(#b01) 'application]
            [(#b10) 'context-specific]
            [(#b11) 'private])
          (if c? 'constructed 'primitive)
          tagnum)))

;; read-length-code : input-port -> nat
(define (read-length-code in)
  (let ([l (read-byte in)])
    (cond [(<= 0 l 127)
           l]
          [else
           (let* ([ll (- l 128)]
                  [lbs (read-bytes ll in)])
             (base256->unsigned lbs))])))
