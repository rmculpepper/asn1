#lang racket/base
(require racket/list
         rackunit
         asn1)

(define (rt1 type value)
  (define der (asn1->bytes/DER type value))
  (define value2 (bytes->asn1/DER type der))
  (check-equal? value2 value "values agree")
  (define der2 (asn1->bytes/DER type value2))
  (check-equal? der2 der "DER agrees"))

(define (rt type . values)
  (when #f (eprintf "* testing ~v\n" type))
  (for ([value values]) (rt1 type value)))

(rt BOOLEAN #t #f)
(rt INTEGER 0 -1 1 -127 127 -128 128 255 256 (expt 17 80) (- (expt 23 81)))
(rt ENUMERATED 0 1 2 127 128 255 256 (expt 2 82))
(rt BIT-STRING
    (bit-string #"" 0) (bit-string #"a" 0)
    (bit-string (bytes #xf0) 4) (bit-string (bytes #xf8) 3))
(rt OCTET-STRING #"" #"abc" #"\0" #"a\0b" (make-bytes #e1e3 65))
(rt NULL #f)

(rt OBJECT-IDENTIFIER '(1 2 3 4) '(1 39))
(rt RELATIVE-OID '() '(99) '(1 2 3) '(389207 372 7099038253 79))
(rt PrintableString "" "a" " " "abc-123+?")
(rt IA5String "" "a" "\0" "\t\r\n" "If I had a time machine\nand if life was a movie scene")
(rt UTF8String "" "a" "\n" "λβ→" "💀")

(rt (SEQUENCE [a INTEGER] [b UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SEQUENCE [a #:explicit 10 INTEGER] [b #:explicit 12345 UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SEQUENCE [a #:implicit 10 INTEGER] [b #:implicit 12345 UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SEQUENCE [a #:explicit 10 INTEGER] [b #:explicit 10 UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SEQUENCE [a INTEGER] [b INTEGER])
    (hasheq 'a 1 'b 2))

(rt (SEQUENCE [a INTEGER] [b INTEGER #:optional])
    (hasheq 'a 1) (hasheq 'a 1 'b 2))
(rt (SEQUENCE [a INTEGER] [b INTEGER #:default 99])
    (hasheq 'a 1 'b 99) (hasheq 'a 1 'b 2))
(rt (SEQUENCE [a INTEGER] [b #:implicit 1 INTEGER #:default 99])
    (hasheq 'a 1 'b 99) (hasheq 'a 1 'b 2))

(rt (SET [a INTEGER] [b UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SET [a #:explicit 10 INTEGER] [b #:explicit 12345 UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SET [a #:implicit 10 INTEGER] [b #:implicit 12345 UTF8String])
    (hasheq 'a 777 'b "hooray"))
(rt (SET [a #:explicit 1 INTEGER] [b #:explicit 2 INTEGER] [c INTEGER])
    (hasheq 'a 1 'b 2 'c 3))

(rt (SET [a INTEGER] [b #:implicit 1 INTEGER #:optional])
    (hasheq 'a 1) (hasheq 'a 1 'b 2))
(rt (SET [a INTEGER] [b #:implicit 1 INTEGER #:default 99])
    (hasheq 'a 1 'b 99) (hasheq 'a 1 'b 2))

(rt (SEQUENCE-OF INTEGER) '() '(0) '(1 2 3) (range 100))
(rt (SET-OF INTEGER) '() '(0) '(1 2 3) (range 1000))

(rt (TAG #:explicit 1 INTEGER) 0 10)
(rt (TAG #:explicit 1234567 INTEGER) 0 10)
(rt (TAG #:explicit #:universal 1234567 INTEGER) 0 10)
(rt (TAG #:explicit #:application 1234567 INTEGER) 0 10)

(rt (TAG #:implicit 1 INTEGER) 1 12)
(rt (TAG #:implicit 1234567 INTEGER) 1 12)
(rt (TAG #:implicit #:universal 1234567 INTEGER) 0 10)
(rt (TAG #:implicit #:application 1234567 INTEGER) 0 10)

(rt (CHOICE [a INTEGER] [b UTF8String])
    '(a 1) '(b "hello"))
(rt (CHOICE [a #:implicit 1 INTEGER] [b #:implicit 2 UTF8String])
    '(a 1) '(b "hello"))
(rt (CHOICE [a INTEGER] [b #:implicit 2 INTEGER])
    '(a 1) '(b 2))
(rt (CHOICE [a #:explicit 1 INTEGER] [b #:explicit 2 INTEGER])
    '(a 1) '(b 2))

(rt (WRAP UTF8String #:encode symbol->string #:decode string->symbol)
    'abc)

(rt (SEQUENCE [a INTEGER] [b #:dependent (cond [(zero? a) INTEGER] [else OCTET-STRING])])
    (hasheq 'a 0 'b 16) (hasheq 'a 1 'b #"abc"))
