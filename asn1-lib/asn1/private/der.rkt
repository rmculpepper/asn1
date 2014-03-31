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
         "types.rkt"
         "der-frame.rkt")
(provide (all-defined-out))

;; DER encoding and decoding

;; ============================================================

;; Encode/Decode hooks

;; DER-encode-hooks : (parameterof (listof (cons Asn1-Type DER-Encode-Hook)))
;; A DER-Encode-Hook is one of
;; - (list 'pre    (Asn1-Type Any -> Any))     -- value-to-value filter
;; - (list 'encode (Asn1-Type Any -> Bytes))   -- provides value bytes
(define DER-encode-hooks (make-parameter null))

;; DER-decode-hooks : (parameterof (listof (cons Asn1-Type DER-Decode-Hook)))
;; A DER-Decode-Hook is one of
;; - (list 'decode (Asn1-Type Bytes -> Any))   -- decodes value bytes
;; - (list 'post   (Asn1-Type Any   -> Any))   -- value-to-value filter
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
  ;; Want to let encode-hook control value encoding, but don't want
  ;; hooks to worry about tag and length.  So, need to search forward
  ;; completely to get base type for tag, but remember when we see an
  ;; encode-hook, and use that instead for encoding.
  (define hooks (DER-encode-hooks))
  (let loop ([type type] [v-in v] [alt-tag alt-tag] [encode-hook-f-in #f])
    ;; Run pre-hooks until we find an encode-hook
    (define pre-hook-f
      (and (not encode-hook-f-in)
           (let ([pre-hook (get-hook 'pre type hooks)])
             (and pre-hook (caddr pre-hook)))))
    (define v (if pre-hook-f (pre-hook-f v-in) v-in))
    (define encode-hook-f
      (or encode-hook-f-in
          (let ([encode-hook (get-hook 'encode type hooks)])
            (and encode-hook (caddr encode-hook)))))
    (define (encode-value)
      (if encode-hook-f
          (encode-hook-f v)
          (DER-encode-value type v)))
    (match type
      [(asn1-type:any)
       ;; Note: no wrapping; encoder must produce whole TLV triple
       ;; alt-tag must be #f; can't implicitly tag an ANY value
       (encode-value)]
      [(asn1-type:base base-type)
       (wrap base-type (encode-value) alt-tag)]
      [(asn1-type:sequence _)
       (wrap 'SEQUENCE (encode-value) alt-tag)]
      [(asn1-type:sequence-of _)
       (wrap 'SEQUENCE (encode-value) alt-tag)]
      [(asn1-type:set _)
       (wrap 'SET (encode-value) alt-tag)]
      [(asn1-type:set-of _)
       (wrap 'SET (encode-value) alt-tag)]
      [(asn1-type:choice elts)
       (match v
         [(list (? symbol? sym) v*)
          (match-define (element _ tag* type* _ _)
            (for/or ([elt (in-list elts)])
              (and (eq? (element-name elt) sym) elt)))
          (loop type* v* tag* encode-hook-f)]
         [_ (error 'asn1-encode "bad value for Choice type\n  value: ~e" v)])]
      [(asn1-type:tag tag* type*)
       ;; Outer implicit tag takes precedence; prefer alt-tag
       (loop type* v (or alt-tag tag*) encode-hook-f)]
      [(asn1-type:explicit-tag _)
       (wrap 'SEQUENCE (encode-value) alt-tag)]
      [(asn1-type:wrap w-type w-pre-encode w-encode _ _)
       (let ([v (if w-pre-encode (w-pre-encode v) v)])
         (loop w-type v alt-tag (or encode-hook-f w-encode)))]
      [(asn1-type:delay promise)
       (loop (force promise) v alt-tag encode-hook-f)])))

(define (DER-encode-value type v)
  (match type
    [(asn1-type:any)
     ;; ANY must be handled by encode hook
     (error 'DER-encode-value
            "no default encoding rule for ANY\n  value: ~e" v)]
    [(asn1-type:base base-type)
     (DER-encode-base* base-type v)]
    [(asn1-type:sequence elts)
     (encode-sequence (filter values (DER-encode-sequence* elts v)))]
    [(asn1-type:sequence-of type*)
     (match v
       [(list 'sequence-of v*s ...)
        (encode-sequence
         (for/list ([v* (in-list v*s)])
           (DER-encode type* v* #f)))]
       [_ (error 'DER-encode-value "bad value for SequenceOf type\n  value: ~e" v)])]
    [(asn1-type:set elts)
     (encode-set (filter values (DER-encode-set* elts v)))]
    [(asn1-type:set-of type*)
     (match v
       [(list 'set-of v*s ...)
        (encode-set
         (for/list ([v* (in-list v*s)])
           (DER-encode type* v* #f)))]
       [_ (error 'DER-encode-value "bad value for SetOf type\n  value: ~e" v)])]
    [(asn1-type:explicit-tag type*)
     (encode-sequence (list (DER-encode type* v)))]
    [(asn1-type:tag _ _)
     (error 'DER-encode-value "bad type\n  type: ~e" type)]
    [(asn1-type:choice _)
     (error 'DER-encode-value "bad type\n  type: ~e" type)]
    [(asn1-type:wrap _ _ _ _ _)
     (error 'DER-encode-value "bad type\n  type: ~e" type)]
    [(asn1-type:delay _)
     (error 'DER-encode-value "bad type\n  type: ~e" type)]))

(define (DER-encode-base* base-type v)
  (define (bad-value [expected #f])
    (error 'DER-encode-value
           "bad value for type\n  type: ~s\n  value: ~e~a"
           base-type v
           (if expected (format "\n  expected: ~a" expected) "")))
  (case base-type
    [(BOOLEAN)
     (encode-boolean v)]
    [(INTEGER)
     (unless (exact-integer? v) (bad-value 'exact-integer?))
     (signed->base256 v)]
    [(BIT-STRING)
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
    [(RELATIVE-OID)
     (unless (and (list? v) (andmap exact-nonnegative-integer? v))
       (bad-value '(listof exact-nonnegative-integer?)))
     (encode-relative-oid v)]
    [(ENUMERATED)
     (unless (exact-nonnegative-integer? v) (bad-value 'exact-integer?))
     (signed->base256 v)]
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
    [(cons 'sequence lvs0)
     (match lvs0
       [(list (list (? symbol?) _) ...)
        (let loop ([elts elts] [lvs lvs0])
          (cond [(and (null? elts) (null? lvs))
                 null]
                [(null? elts)
                 (error 'DER-encode-value
                        "unexpected field in Sequence value\n  value: ~e\n  field: ~s"
                        v (car (car lvs)))]
                [else
                 (match (car elts)
                   [(element name tag* type* option refine)
                    (cond [(and (pair? lvs)
                                (eq? (car (car lvs)) name))
                           (let ([type** (if refine (refine lvs0) type*)])
                             (cons (DER-encode type** (cadr (car lvs)) tag*)
                                   (loop (cdr elts) (cdr lvs))))]
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
    #|
    [(list _ ...)
     (unless (= (length v) (length elts))
       (error 'DER-encode-value "wrong number of elements for Sequence\n  value: ~e" v))
     (for/list ([v* (in-list v)]
                [elt (in-list elts)])
       (match elt
         [(element name tag* type* option refine)
          (DER-encode type* v* tag*)]))]
    |#
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
      [(element name tag* type* option _)
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

(define (encode-bad type value [expected #f] #:msg [msg #f] #:more [more ""])
  (error 'DER-encode-value
         "bad value for type~a\n  type: ~a~a\n  value: ~e~a"
         type
         (if msg (format ";\n ~a" msg) "")
         (if expected (format "\n  expected: ~a" expected) "")
         value
         more))

;; encode-boolean : Boolean -> Bytes
(define (encode-boolean b)
  (unless (boolean? b) (encode-bad 'BOOLEAN b 'boolean?))
  (if b #"\377" #"\0"))

;; encode-bit-string : Bit-String -> Bytes
(define (encode-bit-string bs)
  (match bs
    [(bit-string bits trailing-unused)
     (cond [(zero? (bytes-length bits))
            (unless (zero? trailing-unused)
              (encode-bad 'BIT-STRING bs
                          #:msg "trailing unused bits non-zero for empty bit string"))]
           [else
            (unless (zero? (bitwise-bit-field (bytes-ref bits (sub1 (bytes-length bits)))
                                              0 trailing-unused))
              (encode-bad 'BIT-STRING bs
                          #:msg "trailing unused bits are not 0"))])
     (bytes-append (bytes trailing-unused) bits)]
    [_ (encode-bad 'BIT-STRING bs)]))

;; encode-ia5string : String -> Bytes
(define (encode-ia5string s)
  (unless (ia5string? s) (encode-bad 'IA5String s 'ia5string?))
  (string->bytes/latin-1 s))

;; encode-integer : Exact-Integer -> Bytes
(define (encode-integer n)
  (unless (exact-integer? n) (encode-bad 'INTEGER n 'exact-integer?))
  (signed->base256 n))

;; encode-null : Any -> Bytes
(define (encode-null [_ignored #f])
  #"")

;; encode-object-identifier : (listof (U Nat (List Symbol Nat))) -> Bytes
(define (encode-object-identifier cs)
  (unless (and (list? cs) (andmap exact-nonnegative-integer? cs))
    (encode-bad 'OBJECT-IDENTIFIER cs '(listof exact-nonnegative-integer?)))
  (unless (>= (length cs) 2)
    (encode-bad 'OBJECT-IDENTIFIER cs #:msg "expected at least 2 components"))
  (let ([cs (for/list ([c (in-list cs)])
              (if (list? c) (cadr c) c))])
    (let ([c1 (car cs)]
          [c2 (cadr cs)]
          [cs* (cddr cs)])
      (unless (< c1 3)
        (encode-bad 'OBJECT-IDENTIFIER #:msg "first component too big"))
      (unless (< c2 (if (< c1 2) 40 (- 256 80)))
        (encode-bad 'OBJECT-IDENTIFIER #:msg "second component too big"))
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
(define (encode-relative-oid cs)
  (unless (and (list? cs) (andmap exact-nonnegative-integer? cs))
    (encode-bad 'RELATIVE-OID cs '(listof exact-nonnegative-integer?)))
  (apply bytes-append (map encode-oid-component cs)))

;; encode-octet-string : Bytes -> Bytes
(define (encode-octet-string b)
  (unless (bytes? b) (encode-bad 'OCTET-STRING b 'bytes?))
  b)

;; encode-printable-string : Printable-String -> Bytes
(define (encode-printable-string s)
  (unless (printable-string? s) (encode-bad 'PrintableString s 'printable-string?))
  (string->bytes/latin-1 s))

(define (encode-utf8string s)
  (unless (string? s) (encode-bad 'UTF8String s 'string?))
  (string->bytes/utf-8 s))

;; encode-sequence : (listof Bytes) -> Bytes
(define (encode-sequence lst)
  (unless (and (list? lst) (andmap bytes? lst))
    (encode-bad 'SEQUENCE lst '(listof bytes?)))
  (apply bytes-append lst))

;; encode-set : (listof Bytes) -> Bytes
(define (encode-set lst)
  (unless (and (list? lst) (andmap bytes? lst))
    (encode-bad 'SET lst '(listof bytes?)))
  (apply bytes-append (sort lst bytes<?)))

;; ============================================================

(define (DER-read type in)
  (DER-decode-frame type (read-DER-frame in)))

(define (DER-decode type b)
  (DER-decode-frame type (bytes->DER-frame b)))

(define (DER-decode-frame type frame)
  (define hooks (DER-decode-hooks))
  (match-define (DER-frame tagclass p/c tagn c) frame)
  (let loop ([type type] [decode-hook-f-in #f] [check-whole-tag? #t])
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

    (define decode-hook-f
      (or decode-hook-f-in
          (let ([decode-hook (get-hook 'decode type hooks)])
            (and decode-hook (caddr decode-hook)))))

    (define (decode-value)
      (if decode-hook-f
          (decode-hook-f c)
          (DER-decode-value type c)))

    (define post-hook-f
      (let ([post-hook
             (and (not decode-hook-f-in)
                  (get-hook 'post type hooks))])
        (if post-hook (caddr post-hook) values)))

    (post-hook-f
     (match type
       [(asn1-type:any)
        (if decode-hook-f
            (decode-hook-f (DER-frame->bytes frame))
            (DER-decode-value type (DER-frame->bytes frame)))]
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
            [(cons (and elt0 (element et-name _ et-type _ _)) rest-elts)
             (if (tag-matches elt0 frame)
                 (list et-name (loop et-type decode-hook-f #f))
                 (choice-loop rest-elts))]
            [_ (error 'DER-decode "tag does not match any alternative in Choice")]))]
       [(asn1-type:tag tag type*)
        (unless (equal? tagclass (car tag))
          (error 'DER-decode "tag class mismatch\n  expected: ~s\n  decoded: ~s"
                 (car tag) tagclass))
        (unless (equal? tagn (cadr tag))
          (error 'DER-decode "tag number mismatch\n  expected: ~s\n  decoded: ~s"
                 (cadr tag) tagn))
        (loop type* decode-hook-f #f)]
       [(asn1-type:explicit-tag type*)
        ;; Tag has already been checked by enclosing CHOICE, SEQUENCE, or SET
        (unless (equal? p/c 'constructed)
          (error 'DER-decode "primitive vs constructed mismatch\n  expected: ~s\n  decoded: ~s"
                 'constructed p/c))
        (decode-value)]
       [(asn1-type:wrap w-type _ _ w-decode w-post-decode)
        ((or w-post-decode values)
         (loop w-type (or decode-hook-f w-decode) check-whole-tag?))]
       [(asn1-type:delay promise)
        (loop (force promise) decode-hook-f check-whole-tag?)]))))

;; tag-matches : Element DER-Frame -> Boolean
;; Checks class and tag number for match; FIXME: check p/c
(define (tag-matches elt frame)
  ;; (match-define (element _ et-tag et-type _ _) elt)
  (match-define (DER-frame f-tagclass f-p/c f-tagn _) frame)
  (define et-tags (type->tags elt))
  (for/or ([et-tag (in-list et-tags)])
    ;; FIXME: need to consider p/c !!!
    (or (eq? et-tag #f) ;; #f=ANY matches all tags
        (and (equal? f-tagclass (car et-tag))
             (equal? f-tagn (cadr et-tag))))))

;; DER-decode-value : Asn1-Type Bytes -> Any
;; Note: if type is ANY, c is whole TLV triple; otherwise, just value part.
(define (DER-decode-value type c)
  (match type
    [(asn1-type:any)
     (match-define (DER-frame tagclass p/c tagn content) (bytes->DER-frame c))
     (unless (eq? tagclass 'universal)
       (error 'DER-decode-value "non-universal tag found decoding ANY\n  tag: ~a ~a ~a"
              tagclass tagn p/c))
     (define te (tagn->tag-entry tagn))
     (unless te
       (error 'DER-decode-value "unsupported tag found decoding ANY\n  tag: ~a ~a ~a"
              tagclass tagn p/c))
     (unless (equal? p/c (tag-entry-p/c te))
       (error 'DER-decode-value
              "primitive/constructed mismatch found decoding ANY\n  tag: ~a ~a ~a\n expected: ~a"
              tagclass tagn p/c (tag-entry-p/c te)))
     (define base-type (tag-entry-type te))
     (case base-type
       [(SEQUENCE)
        (cons 'sequence-of
              (for/list ([frame (in-list (bytes->frames content))])
                ;; Note: type = ANY; reuse it
                (DER-decode-frame type frame)))]
       [(SET)
        ;; FIXME: if validating DER of SET, need to check frames are sorted
        (cons 'set-of
              (for/list ([frame (in-list (bytes->frames content))])
                ;; Note: type = ANY; reuse it
                (DER-decode-frame type frame)))]
       [else (DER-decode-base* base-type content)])]
    [(asn1-type:base base-type)
     (DER-decode-base* base-type c)]
    [(asn1-type:sequence elts)
     (DER-decode-sequence* elts (bytes->frames c))]
    [(asn1-type:sequence-of type*)
     (cons 'sequence-of
           (for/list ([frame (in-list (bytes->frames c))])
             (DER-decode-frame type* frame)))]
    [(asn1-type:set elts)
     (DER-decode-set* elts (check-sorted 'Set (bytes->frames c)))]
    [(asn1-type:set-of type*)
     (cons 'set-of
           (for/list ([frame (in-list (check-sorted 'SetOf (bytes->frames c)))])
             (DER-decode-frame type* frame)))]
    [(asn1-type:explicit-tag type*)
     (DER-decode type* (bytes->frames c))]
    [(asn1-type:tag _ _)
     (error 'DER-decode-value "bad type\n  type: ~e" type)]
    [(asn1-type:choice elts)
     (error 'DER-decode-value "bad type\n  type: ~e" type)]
    [(asn1-type:delay _)
     (error 'DER-decode-value "bad type\n  type: ~e" type)]))

(define (DER-decode-base* base-type c)
  (define (bad-value [expected #f])
    (error 'DER-decode-value
           "bad value for type\n  type: ~s\n  value: ~e~a"
           base-type c
           (if expected (format "\n  expected: ~a" expected) "")))
  (case base-type
    [(BOOLEAN)
     (decode-boolean c)]
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
    [(RELATIVE-OID)
     (decode-relative-oid c)]
    [(ENUMERATED)
     (base256->signed c)]
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
    (let loop ([elts elts] [frames frames] [rlvs null])
      (match elts
        [(cons (and elt0 (element et-name et-tag et-type0 et-option et-refine)) rest-elts)

         ;; current element is missing; try to skip
         (define (try-skip rest-frames)
           (match et-option
             ['(optional)
              (loop rest-elts rest-frames rlvs)]
             [(list 'default default-value)
              (loop rest-elts rest-frames
                    (cons (list et-name default-value) rlvs))]
             [#f
              (error 'DER-decode-value
                     "missing field in encoded Sequence\n  field: ~s"
                     et-name)]))

         (match frames
           [(cons (and frame0 (DER-frame f-tagclass f-p/c f-tagn f-c)) rest-frames)
            (cond [(tag-matches elt0 frame0)
                   (define et-type (if et-refine (et-refine rlvs) et-type0))
                   (loop rest-elts rest-frames
                         (cons (list et-name (DER-decode-frame et-type frame0)) rlvs))]
                  [else (try-skip rest-frames)])]
           ['()
            (try-skip '())])]
        ['()
         (if (null? frames)
             (reverse rlvs)
             (error 'DER-decode-value
                    "leftover components in encoded Sequence"))]))))

;; DER-decode-set* : (listof ElementType) (listof Frame)
;;                -> (cons 'set (listof (list Symbol Any)))
(define (DER-decode-set* elts frames)
  (cons 'set
    (let loop ([elts elts] [frames frames])
      (match elts
        [(cons (and elt0 (element et-name _ et-type et-option _)) rest-elts)
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

(define (check-sorted what frames)
  ;; Set/SetOf elements must be sorted
  (let loop ([bs (map DER-frame->bytes frames)])
    (when (and (pair? bs) (pair? (cdr bs)))
      (unless (bytes<? (car bs) (cadr bs))
        (error 'DER-decode-value "invalid order of elements within encoded ~a" what))
      (loop (cdr bs))))
  frames)

;; ----

;; Base type decoders

(define (decode-bad type encoded #:msg [msg #f] #:more [more #f])
  (error 'DER-decode-value
         "bad encoding for type~a\n  type: ~a\n  encoding: ~e~a"
         (if msg (format ";\n ~a" msg) "")
         type encoded more))

;; decode-boolean : Bytes -> Boolean
(define (decode-boolean b)
  (cond [(equal? b #"\377") #t]
        [(equal? b #"\0") #f]
        [else (decode-bad 'BOOLEAN b)]))

;; decode-bit-string : Bytes -> Bit-String
(define (decode-bit-string c)
  (when (zero? (bytes-length c))
    (decode-bad 'BIT-STRING c))
  (let ([trailing-unused (bytes-ref c 0)])
    (unless (<= 0 trailing-unused 7)
      (decode-bad 'BIT-STRING c
                  #:msg "invalid unused bit count"
                  #:more "\n  unused bits: ~s" trailing-unused))
    (bit-string (subbytes c 1 (bytes-length c))
                trailing-unused)))

;; decode-ia5string : Bytes -> String
(define (decode-ia5string bs)
  (define s (bytes->string/latin-1 bs))
  (unless (ia5string? s)
    (decode-bad 'IA5string bs))
  s)

;; decode-integer : Bytes -> Integer
;; Given encoded integer, returns raw integer
(define (decode-integer bs)
  (base256->signed bs))

;; decode-null : Bytes -> #f
(define (decode-null bs)
  (unless (equal? bs #"")
    (decode-bad 'NULL bs))
  #f)

;; decode-object-identifier : Bytes -> (listof Nat)
(define (decode-object-identifier bs)
  (unless (positive? (bytes-length bs))
    (decode-bad 'OBJECT-IDENTIFIER bs))
  (define in (open-input-bytes bs))
  (define b1 (read-byte in))
  (list* (quotient b1 40) (remainder b1 40)
         (decode-oid-components in bs)))
(define (decode-relative-oid bs)
  (decode-oid-components (open-input-bytes bs) bs))
(define (decode-oid-components in bs)
  (let loop ()
    (if (eof-object? (peek-byte in))
        null
        (let ([c (decode-oid-component in bs)])
          (cons c (loop))))))
(define (decode-oid-component in bs)
  (let loop ([c 0])
    (let ([next (read-byte in)])
      (cond [(eof-object? next)
             (decode-bad 'OBJECT-IDENTIFIER bs #:msg "incomplete component")]
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
        (decode-bad 'PrintableString bs))))

(define (decode-utf8string b)
  (if (bytes-utf-8-length b #f)
      (bytes->string/utf-8 b)
      (decode-bad 'UTF8String b)))
