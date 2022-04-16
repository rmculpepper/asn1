;; Copyright 2014-2019 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/struct
         binaryio/integer
         binaryio/bytes
         binaryio/unchecked/reader
         binaryio/unchecked/fixup-port
         "base256.rkt"
         "base.rkt")
(provide (all-defined-out))

;; BER encoding has 3 parts:
;; - tag - class, tag, primitive vs constructed
;; - length - number of contents octets
;; - value

;; DER constraints (frame level)
;; - length is always definite, using most compact encoding

;; ============================================================
;; Tag (class, number, p/c)

;; If tag number <= 30:
;; - one octet: CCENNNNN
;; - bits 7,6 are class (C):
;;   - (0,0)=universal, (0,1)=application, (1,0)=context-sensitive, (1,1)=private
;; - bit 5 is encoding (E):
;;   - 0=primitive, 1=constructed
;; - bits 4-0 are tag number (N)

;; If tag number > 30:
;; - multiple octets: CCE11111 1NNNNNNN ... 1NNNNNNN 0NNNNNNN
;; - Note: appears illegal to encode tag <= 30 in long form in both BER and DER

;; tag->bytes : Tag Boolean -> Bytes
(define (tag->bytes tag tagcons?)
  (define-values (tagclass tagn) (tag->class+index tag))
  (define byte1
    (+ (arithmetic-shift (tagclass->bits tagclass) 6)
       (cond [tagcons?       #b00100000]
             [else           0])
       (cond [(<= tagn 30)   tagn]
             [else           #b00011111])))
  (cond [(<= tagn 30) (bytes byte1)]
        [else (long-tag->bytes byte1 tagn)]))

;; long-tag->bytes : Byte Nat -> Bytes
(define (long-tag->bytes byte1 tagn)
  (define (loop tagn acc)
    (if (zero? tagn)
        acc
        (let ([q (quotient tagn 128)] [r (remainder tagn 128)])
          (loop q (cons (bitwise-ior 128 r) acc)))))
  (apply bytes byte1
         (let ([q (quotient tagn 128)] [r (remainder tagn 128)])
           (loop q (list r)))))

;; ============================================================
;; Length

;; Definite form
;; - Short Definite form (0 <= len <= 127)
;;   - one octet: 0LLLLLLL
;; - Long Definite form:
;;   - multiple octets: 1mmmmmmm {LLLLLLLL}*m
;;   - m must not be 127 (reserved for future extension)

;; Indefinite form
;; - one octet: 10000000
;; - followed by stream of TLVs, ended by TLV encoded as 00 (zero tag, zero length)

;; length->bytes : Nat -> Bytes
(define (length->bytes n)
  (cond [(<= 0 n 127)
         (bytes n)]
        [else
         (let ([nc (unsigned->base256 n)])
           (unless (< (bytes-length nc) 127)
             (BER-error "length too long" "\n  length: ~e" n))
           (bytes-append (bytes (bitwise-ior 128 (bytes-length nc))) nc))]))

;; ----------------------------------------

;; frame : BaseType FrameContent [Tag] -> BER-Frame
(define (frame base-type content [alt-tag #f])
  (define t (or alt-tag (base-type-tag base-type)))
  (BER-frame t content))


;; ============================================================
;; BER Frames (TLV)

;; A BER-Frame is (BER-frame Tag FrameContents)
;; FrameContents = (U Bytes (Listof (U Bytes BER-Frame)))
;; The frame is "constructed" if content is a list, "primitive" if bytes.
(struct BER-frame (tag content)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (f) 'BER-frame)
   (lambda (f)
     (list (unquoted-printing-string
            (format "~v ~v" (BER-frame-tag-class f) (BER-frame-tag-number f)))
           (BER-frame-content f)))))

(define (BER-frame-tag-class f) (tag-class (BER-frame-tag f)))
(define (BER-frame-tag-number f) (tag-index (BER-frame-tag f)))

;; nil-frame? : BER-Frame -> Boolean
(define (nil-frame? f)
  (match f [(BER-frame f-tag #"") (equal? f-tag nil-tag)] [_ #f]))

;; BER-frame/tag{<,<=}? : BER-Frame BER-Frame -> Boolean
(define (BER-frame/tag<? a b)  (tag<?  (BER-frame-tag a) (BER-frame-tag b)))
(define (BER-frame/tag<=? a b) (tag<=? (BER-frame-tag a) (BER-frame-tag b)))

;; ----------------------------------------

;; write-frame : BER-Frame OutputPort Boolean -> Void
(define (write-frame frame out der?)
  (cond [der? (write-frame/definite/fixer frame out)]
        [else (write-frame/indefinite frame out)]))

;; write-frame/indefinite : BER-Frame OutputPort -> Void
;; Write frame using indefinite length encoding for constructed parts.
(define (write-frame/indefinite frame out)
  ;; write-frame : BER-Frame -> Void
  (define (write-frame frame)
    (match-define (BER-frame tag content) frame)
    (write-bytes (tag->bytes tag (list? content)) out)
    (cond [(bytes? content)
           (write-bytes (length->bytes (bytes-length content)) out)
           (write-bytes content out)]
          [else
           (write-byte #b10000000 out)
           (for ([frame (in-list content)])
             (if (bytes? frame) (write-bytes content out) (write-frame frame)))
           (write-bytes #"\0\0" out)]))
  (void (write-frame frame)))

;; write-frame/definite : BER-Frame OutputPort -> Void
;; Write frame using definite length encoding for constructed parts.
(define (write-frame/definite frame out)
  (define content->length (make-caching-content->length))
  ;; write-frame : BER-Frame -> Void
  (define (write-frame frame)
    (match-define (BER-frame tag content) frame)
    (write-bytes (tag->bytes tag (list? content)) out)
    (write-bytes (length->bytes (content->length content)) out)
    (cond [(bytes? content)
           (write-bytes content out)]
          [else
           (for ([frame (in-list content)])
             (if (bytes? frame) (write-bytes frame out) (write-frame frame)))]))
  (void (write-frame frame)))

;; make-caching-content->length : -> FrameContents -> Nat
;; Computes length of content assuming inner frames use definite length encoding.
(define (make-caching-content->length)
  (define lencache (make-hasheq)) ;; BER-Frame => Nat (length)
  (define (frame-prefix-length tag len)
    (+ (bytes-length (tag->bytes tag #f)) (bytes-length (length->bytes len))))
  (define (content->length c)
    (match c
      [(? bytes?) (bytes-length c)]
      [(? null?) 0]
      [(cons c1 c2) (+ (content->length c1) (content->length c2))]
      [(BER-frame f-tag f-c)
       (hash-ref! lencache c
                  (lambda ()
                    (let ([inner-len (content->length f-c)])
                      (+ (frame-prefix-length f-tag inner-len) inner-len))))]))
  content->length)

;; write-frame/definite/naive : BER-Frame OutputPort -> Void
;; Write frame using definite length encoding for constructed parts.
(define (write-frame/definite/naive frame out)
  ;; loop : BER-Frame -> Bytes
  (define (loop frame)
    (match-define (BER-frame tag content) frame)
    (define content-bytes
      (if (bytes? content)
          content
          (apply bytes-append
                 (for/list ([frame (in-list content)])
                   (if (bytes? frame) frame (loop frame))))))
    (bytes-append (tag->bytes tag (list? content))
                  (length->bytes (bytes-length content-bytes))
                  content-bytes))
  (void (write-bytes (loop frame) out)))

;; write-frame/definite/fixer : BER-Frame OutputPort -> Void
;; Write frame using definite length encoding for constructed parts.
(define (write-frame/definite/fixer frame out)
  (define fx (open-fixup-port))

  (define (write-tag tag tagcons?)
    (define-values (tagclass tagn) (tag->class+index tag))
    (cond [(<= tagn 30)
           (write-byte (+ (arithmetic-shift (tagclass->bits tagclass) 6)
                          (if tagcons? #b00100000 0)
                          tagn)
                       fx)]
          [else (write-bytes (tag->bytes tag tagcons?) fx)]))

  (define (write-length n)
    (cond [(<= 0 n 127) (write-byte n fx)]
          [else (write-bytes (length->bytes n) fx)]))

  ;; loop : (U Bytes BER-Frame) -> Bytes
  (define (loop frame)
    (match frame
      [(? bytes?)
       (write-bytes frame fx)]
      [(BER-frame tag (? bytes? content))
       (write-tag tag #f)
       (write-length (bytes-length content))
       (write-bytes content fx)]
      [(BER-frame tag (? list? content))
       (write-tag tag #t)
       (push-fixup fx)
       (for ([frame (in-list content)]) (loop frame))
       (pop-fixup fx length->bytes)]))
  (loop frame)
  (fixup-port-flush fx out))


;; ----------------------------------------
;; Reader

;; The BER-Frames produced by the reader (`read-frame`) have a restricted shape:
;;
;; FrameContents = (U Bytes (Listof BER-Frame))

;; read-frame-header : BinaryReader Boolean -> (values Tag Boolean (U Nat #f))
(define (read-frame-header br der?)

  ;; read-tag : -> (values Tag Boolean)
  (define (read-tag)
    (define tag (b-read-byte br))
    (define cons? (bitwise-bit-set? tag 5))
    (define tagclass (bits->tagclass (bitwise-bit-field tag 6 8)))
    (define tagnum0 (bitwise-and tag 31))
    (define tagnum
      (if (<= tagnum0 30)
          tagnum0
          (let ([tagnum (read-long-tag)])
            (unless (> tagnum 30)
              (BER-error "found long tag form where short form would suffice"
                         "\n  tag: ~a ~a (~a)"
                         tagclass tagnum (if cons? 'constructed 'primitive)))
            tagnum)))
    (values (make-tag tagclass tagnum) cons?))

  ;; read-long-tag : -> Nat
  (define (read-long-tag)
    (let loop ([c 0])
      (let ([next (b-read-byte br)])
        (cond [(< next 128)
               (+ next (arithmetic-shift c 7))]
              [else
               (loop (+ (- next 128) (arithmetic-shift c 7)))]))))

  ;; read-length : -> (U Nat #f)
  (define (read-length)
    (let ([l (b-read-byte br)])
      (cond [(< l 128) l]
            [(= l 128) (if der? (DER-error "indefinite length encoding") #f)]
            [(= l 255) (BER-error "invalid length (reserved encoding)")]
            [else
             (let ([ll (- l 128)])
               (define len (b-read-integer br ll #f))
               (when der?
                 (when (< len 128)
                   (DER-error "found long definite length encoding where short would suffice"
                              "\n  length: ~e" len))
                 (when (< (integer-bytes-length len #f) ll)
                   (DER-error "excess bytes used in long definite length encoding"
                              "\n  length: ~e\n  bytes used: ~e" len ll)))
               len)])))

  (define-values (tag cons?) (read-tag))
  (define len (read-length))
  (let ([limit (b-get-limit br)])
    (when (and len limit (> len limit))
      (BER-error "inner length exceeds limit"
                 "\n  length: ~e\n  limit: ~e" len (b-get-limit br))))
  (when (equal? tag nil-tag)
    (when cons?
      ;; FIXME
      (BER-error "nil tag with constructed flag set"))
    (unless (eqv? len 0)
      (BER-error "nil tag with non-zero length"
                 "\n  tag: ~a\n  length: ~s" (display-tag tag) len)))
  (values tag cons? len))


;; read-frame : -> BER-Frame
(define (read-frame br der?)

  ;; read-frame-content : Tag Boolean (U Nat #f) -> BER-Frame
  (define (read-frame-content tag cons? len)
    (define content
      (cond [(and cons? len)
             (b-push-limit br len)
             (begin0 (read-frames-until-limit)
               (b-pop-limit br))]
            [(and cons? (not len))
             (read-frames-until-nil)]
            [else (bytes->immutable-bytes (b-read-bytes br len))]))
    (BER-frame tag content))

  ;; read-frames-until-limit : -> (Listof BER-Frame)
  (define (read-frames-until-limit)
    (let loop ([acc null])
      (if (zero? (b-get-limit br))
          (reverse acc)
          (loop (cons (read-frame) acc)))))

  ;; read-frames-until-nil : -> (Listof BER-Frame)
  (define (read-frames-until-nil)
    (let loop ([acc null])
      (define next (read-frame))
      (cond [(nil-frame? next) (reverse acc)]
            [else (loop (cons next acc))])))

  ;; read-frame : -> BER-Frame
  (define (read-frame)
    (define-values (tag cons? len) (read-frame-header br der?))
    (read-frame-content tag cons? len))

  (read-frame))

(define asn1-error-handler
  (make-binary-reader-error-handler
   #:error (lambda (br who fmt . args)
             (apply BER-error "error reading BER frame structure;\n " fmt args))
   ;; Since the main client of the asn1 library is the crypto library, default
   ;; to not showing data in exns.
   #:show-data? (lambda (br who) #f)))

(define (make-asn1-binary-reader in #:limit [limit #f])
  (make-binary-reader in #:limit limit #:error-handler asn1-error-handler))
