;; Copyright 2014-2017 Ryan Culpepper
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
         "base.rkt"
         "types.rkt"
         "ber-frame.rkt")
(provide (all-defined-out))

;; BER and BER encoding and decoding

;; DER constraints (from Dubuisson, Table 19.1-2)
;; - use primitive encoding for all base types other than sequence, set
;; - BOOLEAN true encoded as 255
;; - BIT STRING
;;   - unused bits must be 0
;;   - if type includes named bit list, trailing 0 bits are not encoded
;;   - if type includes SIZE constraint, given size is respected
;;   - if no 1 bits, encoded as one octet: L=1, V=0
;; - GeneralizedTime, UTCTime
;;   - seconds not required, no dot if no fractional second
;;   - must be in UTC ("Z" suffix")
;;   - must use dot as decimal separator
;; - SEQUENCE, SET
;;   - fields equal to default value are not encoded
;; - SET
;;   - components ordered in ascending order of tag
;;     (actual tag, for CHOICE-typed fields)
;; - SET OF
;;   - elements ordered in ascending order of "encodings"
;;     (FIXME: ambiguous whether encoding is V or whole TLV)


;; ============================================================

;; - Base-Type            ~ bytes, string, integer, etc
;; - (SequenceOf T)       ~ (list V[T] ...)
;; - (SetOf T)            ~ (list V[T] ...)
;; - (Sequence [L T] ...) ~ (hasheq [L => V[T]] ...)
;; - (Set [L T] ...)      ~ (hasheq [L => V[T]) ...)
;; - (Choice [L T] ...)   ~ (list L V[T])

;; ============================================================
;; Encoding

;; BER-encode : Type Any Boolean -> BER-Frame
(define (BER-encode type v #:der? [der? #f])

  ;; encode-frame : Type Any Tag/#f -> BER-Frame
  (define (encode-frame type v alt-tag)
    (match type
      [(asn1-type:any)
       ;; Note: alt-tag must be #f; can't implicitly tag an ANY value
       (unless (BER-frame? v) (encode-bad 'ANY v 'BER-frame?))
       v]
      [(asn1-type:base base-type)
       (let ([c (BER-encode-base base-type v)])
         (frame base-type c alt-tag))]
      [(asn1-type:sequence cts)
       (unless (and (hash? v) (for/and ([key (in-hash-keys v)]) (symbol? key)))
         (encode-bad type v '(hash/c symbol? any/c)))
       (let ([c (encode-components cts v 'Sequence)])
         (frame 'SEQUENCE c alt-tag))]
      [(asn1-type:sequence-of type*)
       (unless (list? v) (encode-bad type v 'list?))
       (let ([c (for/list ([elem (in-list v)]) (encode-frame type* elem #f))])
         (frame 'SEQUENCE c alt-tag))]
      [(asn1-type:set components)
       (unless (and (hash? v) (for/and ([key (in-hash-keys v)]) (symbol? key)))
         (encode-bad type v '(hash/c symbol? any/c)))
       (let* ([c-frames (encode-components components v 'Set)]
              [c-frames (if der? (sort c-frames BER-frame/tag<?) c-frames)])
         (frame 'SET c-frames alt-tag))]
      [(asn1-type:set-of type*)
       (unless (list? v) (encode-bad type v 'list?))
       (let* ([c-frames (for/list ([elem (in-list v)]) (encode-frame type* elem #f))]
              [c-frames (if der? (sort (map frame->DER c-frames) bytes<?) c-frames)])
         ;; FIXME: detect duplicates?
         (frame 'SET c-frames alt-tag))]
      [(asn1-type:choice variants)
       (match v
         [(list (? symbol? sym) v*)
          (match (variants-name-assq sym variants)
            [(variant _ type* _) (encode-frame type* v* #f)]
            [_ (encode-bad type v '(list/c symbol? any/c)
                           #:msg "symbol tag does not match any variant name")])]
         [_ (encode-bad type v '(list/c symbol? any/c))])]
      [(asn1-type:implicit-tag tag* type*)
       ;; Outer implicit tag takes precedence; prefer alt-tag
       (encode-frame type* v (or alt-tag tag*))]
      [(asn1-type:explicit-tag tag* type*)
       (frame 'SEQUENCE (list (encode-frame type* v #f)) (or alt-tag tag*))]
      [(asn1-type:wrap type* pre-encode _)
       (let ([v* (if pre-encode (pre-encode v) v)])
         (encode-frame type* v* alt-tag))]
      [(asn1-type:delay promise)
       (encode-frame (force promise) v alt-tag)]
      [_ (error 'BER-encode "internal error: unknown type: ~e" type)]))

  ;; encode-components : (listof Component) Hash[Symbol => Any] Symbol -> (listof BER-Frame)
  (define (encode-components cts h kind)
    ;; FIXME: check for unexpected fields?
    (filter values (for/list ([ct (in-list cts)]) (encode-component ct h kind))))

  ;; encode-component : Component Hash[Symbol => Any] Symbol -> BER-Frame/#f
  (define (encode-component ct h kind)
    (match-define (component name type option refine _) ct)
    (define type* (if refine (refine h) type))
    (define default (match option [(list 'default default) default] [_ none]))
    (define value (hash-ref h name default))
    (cond [(and option (equal? value default))
           #f]
          [(eq? value none)
           (asn1-error "missing required field in ~s value\n  field: ~s\n  value: ~e"
                       kind name h)]
          [else (encode-frame type* value #f)]))

  (encode-frame type v #f))

;; BER-encode-base : Symbol Any -> Bytes
(define (BER-encode-base base-type v)
  (define (bad-value expected) (encode-bad base-type v expected))
  (case base-type
    [(BOOLEAN)
     (unless (boolean? v) (bad-value v 'boolean?))
     (if v #"\377" #"\0")]
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
     (apply bytes-append (map encode-oid-component v))]
    [(ENUMERATED)
     (unless (exact-nonnegative-integer? v) (bad-value 'exact-integer?))
     (signed->base256 v)]
    ;; Sequence[Of], Set[Of]
    [(PrintableString)
     (unless (asn1-printable-string? v) (bad-value 'asn1-printable-string?))
     (string->bytes/latin-1 v)]
    ;; T61String
    [(IA5String)
     (unless (ascii-string? v) (bad-value 'ascii-string?))
     (string->bytes/latin-1 v)]
    ;; UTCTime
    [(UTF8String)
     (unless (string? v) (bad-value 'string?))
     (string->bytes/utf-8 v)]
    [else (error 'BER-encode-base "internal error: unsupported base type\n  type: ~s" base-type)]))

(define none (gensym))

;; frame->DER : BER-Frame -> Bytes
(define (frame->DER frame)
  (define out (open-output-bytes))
  (write-BER-frame frame out #:der? #t)
  (get-output-bytes out))

;; ----------------------------------------
;; Base type encoders

(define (encode-bad type value [expected #f] #:msg [msg #f] #:more [more ""])
  (asn1-error "bad value for type~a\n  type: ~a~a\n  value: ~e~a"
              (if msg (format ";\n ~a" msg) "")
              type
              (if expected (format "\n  expected: ~a" expected) "")
              value
              more))

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
    [_ (encode-bad 'BIT-STRING bs 'bit-string?)]))

;; encode-object-identifier : (listof (U Nat (List Symbol Nat))) -> Bytes
(define (encode-object-identifier cs)
  (unless (and (list? cs) (andmap exact-nonnegative-integer? cs))
    (encode-bad 'OBJECT-IDENTIFIER cs '(listof exact-nonnegative-integer?)))
  (match cs
    [(list* c1 c2 cs*)
     (unless (< c1 3)
       (encode-bad 'OBJECT-IDENTIFIER #:msg "first component too big"))
     (unless (< c2 (if (< c1 2) 40 (- 256 80)))
       (encode-bad 'OBJECT-IDENTIFIER #:msg "second component too big"))
     (apply bytes-append
            (bytes (+ (* 40 c1) c2))
            (map encode-oid-component cs*))]
    [_ (encode-bad 'OBJECT-IDENTIFIER cs #:msg "expected at least 2 components")]))

;; encode-oid-component : Nat -> Bytes
(define (encode-oid-component c)
  (define (loop c acc)
    (if (zero? c)
        acc
        (let-values ([(q r) (quotient/remainder c 128)])
          (loop q (cons (bitwise-ior 128 r) acc)))))
  (apply bytes
         (let-values ([(q r) (quotient/remainder c 128)])
           (loop q (list r)))))

(define (encode-bmp-string s)
  (define b (make-bytes (* 2 (string-length s))))
  (for ([i (in-range (string-length s))])
    (define si (char->integer (string-ref s i)))
    (unless (or (<= #x0000 si #xd7ff) (<= #xe000 si #xffff))
      (encode-bad 'BMPString s))
    (integer->integer-bytes si 2 #f #t b (* i 2)))
  s)

(define (encode-universal-string s)
  (define b (make-bytes (* 4 (string-length s))))
  (for ([i (in-range (string-length s))])
    (integer->integer-bytes (char->integer (string-ref s i)) 2 #f #t b (* i 4)))
  s)

;; ============================================================
;; Decoding

;; BER-decode : Type BER-Frame Boolean -> Any
(define (BER-decode type frame #:der? [der? #f])

  ;; decode-frame : Type BER-Frame Boolean -> Any
  (define (decode-frame type frame check-tag?)
    (match-define (BER-frame tag c) frame)
    (define (check-tag want-tag)
      (when (and check-tag? (not (equal? tag want-tag)))
        (asn1-error "tag mismatch\n  expected: ~a\n  decoded: ~a"
                    (display-tag want-tag) (display-tag tag))))
    (define (check-cons? base-type)
      (unless (base-type-cons-ok? base-type der? (list? c))
        (asn1-error "expected ~a encoding\n  type: ~e"
                    (if (list? c) "primitive" "constructed") type)))
    (define (error/need-cons)
      (asn1-error "expected constructed encoding\n  type: ~e" type))
    (match type
      [(asn1-type:any)
       frame]
      [(asn1-type:base base-type)
       (begin (check-tag (base-type-tag base-type)) (check-cons? base-type))
       (BER-decode-base base-type c der?)]
      [(asn1-type:sequence components)
       (begin (check-tag (base-type-tag 'SEQUENCE)) (unless (list? c) (error/need-cons)))
       (decode-sequence components c type)]
      [(asn1-type:sequence-of type*)
       (begin (check-tag (base-type-tag 'SEQUENCE)) (unless (list? c) (error/need-cons)))
       (for/list ([c-frame (in-list c)]) (decode-frame type* c-frame #t))]
      [(asn1-type:set components)
       (begin (check-tag (base-type-tag 'SET)) (unless (list? c) (error/need-cons)))
       (when (and der? (not (sorted? c BER-frame/tag<?)))
         (DER-error "unsorted elements decoding Set" "\n  type: ~e" type))
       (decode-set components c type)]
      [(asn1-type:set-of type*)
       (begin (check-tag (base-type-tag 'SET)) (unless (list? c) (error/need-cons)))
       ;; FIXME: in DER, elements must be sorted
       (for/list ([c-frame (in-list c)]) (decode-frame type* c-frame #t))]
      [(asn1-type:choice variants)
       (match (variants-tag-assq tag variants)
         [(variant name type* _)
          (list name (decode-frame type* frame #f))]
         [_ (asn1-error "decoded tag does not match any variant\n  tag: ~a\n  type: ~e"
                        (display-tag tag) type)])]
      [(asn1-type:implicit-tag want-tag type*)
       (check-tag want-tag)
       (decode-frame type* frame #f)]
      [(asn1-type:explicit-tag want-tag type*)
       (begin (check-tag want-tag) (unless (list? c) (error/need-cons)))
       (match c
         [(list c-frame) (decode-frame type* c-frame #t)]
         [_ (BER-error "expected single frame for explicitly tagged contents"
                       "\n  type: ~e\n  frames: ~s" type (length c))])]
      [(asn1-type:wrap type* _ post-decode)
       (if post-decode
           (post-decode (decode-frame type* frame check-tag?))
           (decode-frame type* frame check-tag?))]
      [(asn1-type:delay promise)
       (decode-frame (force promise) frame check-tag?)]
      ))

  ;; decode-sequence : (Listof Component) (Listof Frame) Type -> Hasheq[Symbol => Any]
  (define (decode-sequence cts frames type)
    (define-values (unused-frames h)
      (for/fold ([frames frames] [h (hasheq)]) ([ct (in-list cts)])
        (match-define (component ct-name ct-type0 ct-option ct-refine ct-tags) ct)
        (define (try-skip)
          (match ct-option
            [(list 'optional) (values frames h)]
            [(list 'default default) (values frames (hash-set h ct-name default))]
            [#f (BER-error "missing required field in encoded SEQUENCE"
                           "\n  type: ~e\n  field: ~s" type ct-name)]))
        (match frames
          [(cons (and frame (BER-frame tag _)) rest-frames)
           (cond [(or (member tag ct-tags) (memq #f ct-tags))
                  (define ct-type (if ct-refine (ct-refine h) ct-type0))
                  (define value (decode-frame ct-type frame #t))
                  (check-explicit-default ct-name ct-option value type)
                  (values rest-frames (hash-set h ct-name value))]
                 [else (try-skip)])]
          ['() (try-skip)])))
    (unless (null? unused-frames)
      (BER-error "leftover components in encoded SEQUENCE" "\n  type: ~e" type))
    h)

  ;; decode-set : (Listof Component) (Listof Frame) Type -> Hasheq[Symbol => Any]
  (define (decode-set cts frames type)
    (define-values (unused-frames h)
      (for/fold ([frames frames] [h (hasheq)]) ([ct (in-list cts)])
        (match-define (component ct-name ct-type ct-option _ ct-tags) ct)
        (cond [(for/first ([frame (in-list frames)]
                           #:when (member (BER-frame-tag frame) ct-tags))
                 frame)
               => (lambda (frame)
                    (define value (decode-frame ct-type frame #t))
                    (check-explicit-default ct-name ct-option value type)
                    (values (remq frame frames) (hash-set h ct-name value)))]
              [else ;; current element is missing; try to skip
               (match ct-option
                 [(list 'optional)
                  (values frames h)]
                 [(list 'default default)
                  (values frames (hash-set h ct-name default))]
                 [#f (BER-error "missing required field in encoded SET"
                                "\n  type: ~e\n  field: ~s" type ct-name)])])))
    (unless (null? unused-frames)
      (BER-error "leftover components in encoded SET" "\n  type: ~e" type))
    h)

  ;; check-explicit-default : Symbol MaybeOption Any Type -> Void or (error)
  (define (check-explicit-default ct-name ct-option value type)
    (when der?
      (match ct-option
        [(list 'default default)
         (when (equal? value default)
           (DER-error "default field value encoded"
                      "\n  type: ~e\n  field: ~s" type ct-name))]
        [_ (void)])))

  ;; --
  (decode-frame type frame #t))

;; BER-decode-base : BaseType FrameContents Boolean -> Any
(define (BER-decode-base base-type c der?)
  (define (gather) (base-contents->bytes base-type c))
  (case base-type
    [(BOOLEAN)          (decode-boolean c der?)]
    [(INTEGER)          (base256->signed c)]
    [(BIT-STRING)       (decode-bit-string c der?)]
    [(OCTET-STRING)     (gather)]
    [(NULL)             #f]
    [(OBJECT-IDENTIFIER)(decode-object-identifier c)]
    [(RELATIVE-OID)     (decode-relative-oid c)]
    [(ENUMERATED)       (base256->signed c)]
    [(PrintableString)  (decode-printable-string (gather))]
    ;; T61String
    [(IA5String)        (decode-ia5string (gather))]
    ;; UTCTime
    [(UniversalString)  (decode-universal-string (gather))]
    [(BMPString)        (decode-bmp-string (gather))]
    [(UTF8String)       (decode-utf8-string (gather))]
    ;; GeneralizedTime
    [else (error 'BER-decode-base "internal error: unexpected base type\n  type: ~s" base-type)]))

;; base-contents->bytes : BaseType (U Bytes (Listof BER-Frame)) -> Bytes
(define (base-contents->bytes base-type c)
  (if (bytes? c) c (apply bytes-append (base-contents->bytes-list base-type c))))

;; base-contents->bytes-list : Symbol (U Bytes (Listof BER-Frame)) -> (Listof Bytes)
(define (base-contents->bytes-list base-type c)
  (define tag (base-type-tag base-type))
  (let loop ([c c] [onto null])
    (match c
      [(? bytes?) (cons c onto)]
      [(cons ca cb) (loop ca (loop cb onto))]
      [(? null?) onto]
      [(BER-frame f-tag f-c)
       (unless (equal? f-tag tag)
         (BER-error "tag mismatch decoding constructed base type"
                    "\n  type: ~e\n  expected: ~a\n  decoded: ~a"
                    base-type (display-tag tag) (display-tag f-tag)))
       (loop f-c)])))

;; sorted? : (Listof X) (X X -> Boolean) -> Boolean
(define (sorted? xs <?)
  (if (pair? xs)
      (let loop ([x0 (car xs)] [xs (cdr xs)])
        (if (pair? xs)
            (and (<? x0 (car xs)) (loop (car xs) (cdr xs)))
            #t))
      #t))

;; ----

;; Base type decoders

(define (decode-bad type encoded #:msg [msg #f] #:more [more #f])
  (asn1-error "bad encoding for type~a\n  type: ~a\n  encoding: ~e~a"
              (if msg (format ";\n ~a" msg) "")
              type encoded more))

;; decode-boolean : Bytes -> Boolean
(define (decode-boolean b der?)
  (cond [(equal? b #"\0") #f]
        [(equal? b #"\377") #t]
        [(and (not der?) (= (bytes-length b) 1)) #t]
        [else (decode-bad 'BOOLEAN b)]))

;; decode-bit-string : Bytes -> Bit-String
(define (decode-bit-string c der?)
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
  (unless (ascii-string? s)
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

;; decode-printable-string : Bytes -> Printable-String
(define (decode-printable-string bs)
  (let ([s (bytes->string/latin-1 bs)])
    (unless (asn1-printable-string? s) (decode-bad 'PrintableString bs))
    s))

(define (decode-utf8-string b)
  (if (bytes-utf-8-length b #f)
      (bytes->string/utf-8 b)
      (decode-bad 'UTF8String b)))

(define (decode-bmp-string b)
  (unless (even? (bytes-length b))
    (BER-error "encoded BMPString length not a multiple of 2"))
  (define s (make-string (quotient (bytes-length b) 2)))
  (for ([i (in-range (string-length s))])
    (string-set! s i (integer->char (integer-bytes->integer b #f #t (+ i i) (+ i i 2)))))
  s)

(define (decode-universal-string b)
  (unless (zero? (remainder (bytes-length b) 4))
    (BER-error "encoded UniversalString length not a multiple of 4"))
  (define s (make-string (quotient (bytes-length b) 4)))
  (for ([i (in-range (string-length s))])
    (string-set! s i (integer->char (integer-bytes->integer b #f #t (* i 4) (+ (* i 4) 4)))))
  s)
