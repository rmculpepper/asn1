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
         binaryio/integer
         binaryio/bytes
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

;; ----

;; read-tag : InputPort -> (values Tag Boolean)
(define (read-tag in)
  (define tag (read-byte in))
  (define cons? (bitwise-bit-set? tag 5))
  (define tagclass (bits->tagclass (bitwise-bit-field tag 6 8)))
  (define tagnum0 (bitwise-and tag 31))
  (define tagnum
    (if (<= tagnum0 30)
        tagnum0
        (let ([tagnum (read-long-tag in)])
          (unless (> tagnum 30)
            (BER-error "long tag format used for short tag"
                       "\n  tag: ~s ~s ~s"
                       tagclass (if cons? 'constructed 'primitive) tagnum))
          tagnum)))
  (values (make-tag tagclass tagnum) cons?))

;; read-long-tag : InputPort -> Nat
(define (read-long-tag in)
  (let loop ([c 0])
    (let ([next (read-byte in)])
      (cond [(eof-object? next)
             (error 'read-tag "incomplete tag")]
            [(< next 128)
             (+ next (arithmetic-shift c 7))]
            [else
             (loop (+ (- next 128) (arithmetic-shift c 7)))]))))

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

;; ----

;; read-length : InputPort Boolean -> (U Nat #f)
(define (read-length in der?)
  (match (read-length* in)
    [(? exact-nonnegative-integer? len)
     len]
    [#f
     (when der? (DER-error "indefinite length encoding"))
     #f]
    [(cons len llen)
     (when (and der? (< len 128))
       (DER-error "long definite length encoding" "\n  length: ~e" len))
     (when (and der? (< (integer-bytes-length len) llen))
       (DER-error "long definite length encoding" "\n  length: ~e\n  octets used: ~e" len llen))
     len]))

;; read-length* : InputPort -> (U Nat #f (cons Nat Nat))
(define (read-length* in)
  (let ([l (read-byte in)])
    (cond [(<= 0 l 127) (cons l 1)]
          [else
           (define ll (- l 128))
           (cond [(zero? ll) #f]
                 [else (cons (read-integer ll #f in) ll)])])))

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
(struct BER-frame (tag content) #:transparent)

;; nil-frame? : BER-Frame -> Boolean
(define (nil-frame? f)
  (match f [(BER-frame f-tag #"") (equal? f-tag nil-tag)]))

;; BER-frame/tag{<,<=}? : BER-Frame BER-Frame -> Boolean
(define (BER-frame/tag<? a b)  (tag<?  (BER-frame-tag a) (BER-frame-tag b)))
(define (BER-frame/tag<=? a b) (tag<=? (BER-frame-tag a) (BER-frame-tag b)))

;; ----------------------------------------

;; write-BER-frame : BER-Frame [OutputPort] -> Void
(define (write-BER-frame frame [out (current-output-port)] #:der? [der? #f])
  (cond [der? (write-BER-frame/definite frame out)]
        [else (write-BER-frame/indefinite frame out)]))

;; write-BER-frame/indefinite : BER-Frame [OutputPort] -> Void
;; Write frame using indefinite length encoding for constructed parts.
(define (write-BER-frame/indefinite frame [out (current-output-port)])
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

;; write-BER-frame/definite : BER-Frame [OutputPort] -> Void
;; Write frame using definite length encoding for constructed parts.
(define (write-BER-frame/definite frame [out (current-output-port)])
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
             (if (bytes? frame) (write-bytes content out) (write-frame frame)))]))
  (void (write-frame frame)))

;; make-caching-content->length : -> (U Bytes (Listof (U Bytes BER-Frame))) -> Nat
;; Computes length of content assuming inner frames use definite length encoding.
(define (make-caching-content->length)
  (define lencache (make-hasheq)) ;; BER-Frame => Nat (length)
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

;; frame-prefix-length : Tag Nat -> Nat
(define (frame-prefix-length tag len)
  (+ (bytes-length (tag->bytes tag #f)) (bytes-length (length->bytes len))))

;; write-BER-frame/naive : BER-Frame [OutputPort] -> Void
;; Write frame using definite length encoding for constructed parts.
(define (write-BER-frame/naive frame [out (current-output-port)])
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

;; ----------------------------------------

;; read-BER-frame : InputPort [Boolean] -> BER-Frame
(define (read-BER-frame [in (current-input-port)] #:der? [der? #f] #:limit [limit +inf.0])
  (define limitb (box limit)) ;; FIXME: make parameter for default limit?
  (read-BER (make-fuel-input-port in limitb) der? limitb))

;; read-BER : FuelInputPort Boolean (Box Nat/inf) -> BER-Frame
(define (read-BER in der? limitb)
  (define-values (tag cons?) (read-tag in))
  (define len (read-length in der?))
  (when (and len (> len (unbox limitb)))
    (BER-error "inner length exceeds limit"
               "\n  length: ~e\n  limit: ~e" len (unbox limitb)))
  (define content
    (cond [(and cons? len)
           (define saved-limit (- (unbox limitb) len))
           (set-box! limitb len)
           (begin0 (read-frames-until-eof in der? limitb)
             (set-box! limitb saved-limit))]
          [(and cons? (not len))
           (read-frames-until-nil in der? limitb)]
          [else (read-bytes* len in)]))
  (BER-frame tag content))

;; read-frames-until-eof : InputPort Boolean (Box Nat/inf) -> (Listof BER-Frame)
(define (read-frames-until-eof in der? limitb)
  (let loop ([acc null])
    (if (zero? (unbox limitb))
        (reverse acc)
        (loop (cons (read-BER in der? limitb) acc)))))

;; read-frames-until-nil : InputPort Boolean (Box Nat/inf) -> (Listof BER-Frame)
(define (read-frames-until-nil in der? limitb)
  (let loop ([acc null])
    (define next (read-BER in der? limitb))
    (cond [(nil-frame? next) (reverse acc)]
          [else (loop (cons next acc))])))

;; ----------------------------------------

;; BER-frame->bytes : DER-Frame -> Bytes
(define (BER-frame->bytes frame [der? #f])
  (let ([out (open-output-bytes)])
    (write-BER-frame/definite frame out)
    (get-output-bytes out)))

;; bytes->BER-frame : Bytes [Boolean] -> BER-Frame
(define (bytes->BER-frame ber [der? #f])
  (define in (open-input-bytes ber))
  (begin0 (read-BER-frame in der?)
    (unless (eof-object? (peek-char in))
      (BER-error "bytes left over after one TLV frame"))))

;; ----------------------------------------

;; make-fuel-input-port : InputPort (Boxof Nat/+inf.0) -> InputPort
(define (make-fuel-input-port port fuel)
  (define (do-read buf)
    (let ([count (let ([ufuel (unbox fuel)] [buflen (bytes-length buf)])
                   (if (<= buflen ufuel) buflen ufuel))])
      (if (zero? count)
          eof
          (let ([n (read-bytes-avail!* buf port 0 count)])
            (cond [(eq? n 0) (wrap-evt port (lambda (x) 0))]
                  [(number? n) (set-box! fuel (- (unbox fuel) n)) n]
                  [(procedure? n) (set-box! fuel (sub1 (unbox fuel))) n]
                  [else n])))))
  (make-input-port (object-name port) do-read #f void))
