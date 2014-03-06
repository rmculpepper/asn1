#lang racket/base
(provide (all-defined-out))

(define (unsigned->base256 n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'unsigned->base256 "exact-nonnegative-integer?" n))
  (nonnegative-integer->base256 n #f))

(define (signed->base256 n)
  (unless (exact-integer? n)
    (raise-argument-error 'signed->base256 "exact-integer?" n))
  (if (negative? n)
      (negative-integer->base256 n)
      (nonnegative-integer->base256 n #t)))

(define (nonnegative-integer->base256 n as-signed?)
  (if (zero? n)
      #"0"
      (apply bytes
             (let loop ([n n] [acc null])
               (if (zero? n)
                   (if (and as-signed? (> (car acc) 127))
                       (cons 0 acc)
                       acc)
                   (let ([r (bitwise-bit-field n 0 8)]
                         [q (arithmetic-shift n -8)])
                     (loop q (cons r acc))))))))

(define (negative-integer->base256 n)
  (apply bytes
         (let loop ([n n] [acc null])
           (cond [(<= -128 n -1)
                  (cons (+ 256 n) acc)]
                 [else
                  (let* ([b (bitwise-bit-field n 0 8)]
                         [q (arithmetic-shift n -8)])
                    (loop q (cons b acc)))]))))

(define (base256->unsigned bs)
  (for/fold ([n 0]) ([b (in-bytes bs)])
    (+ (arithmetic-shift n 8) b)))

(define (base256->signed bs)
  (if (and (positive? (bytes-length bs)) (> (bytes-ref bs 0) 127))
      (- (base256->unsigned bs)
         (arithmetic-shift 1 (* 8 (bytes-length bs))))
      (base256->unsigned bs)))
