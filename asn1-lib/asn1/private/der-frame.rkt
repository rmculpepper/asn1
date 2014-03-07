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
         "base256.rkt"
         "base-types.rkt")
(provide (all-defined-out))

;; DER frame (the TLV triple) operations


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
        [(private)          #b11000000]
        [else (error 'get-tag-bytes "bad class: ~e" class)])
      (case p/c
        [(primitive)   0]
        [(constructed) #b00100000]
        [else (error 'get-tag-bytes "bad p/c: ~e" p/c)])
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
