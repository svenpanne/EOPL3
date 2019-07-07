#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.1

(define N 10)

(define zero-bignum
  (lambda ()
    '()))

(define is-zero?-bignum
  (lambda (n)
    (null? n)))

(define successor-bignum
  (lambda (n)
    (cond ((null? n) '(1))
          ((= (car n) (- N 1)) (cons 0 (successor-bignum (cdr n))))
          (else (cons (+ (car n) 1) (cdr n))))))

(define predecessor-bignum
  (lambda (n)
    (cond ((null? n) (eopl:error 'predecessor-bignum "zero.~%"))
          ((equal? n '(1)) '())
          ((zero? (car n)) (cons (- N 1) (predecessor-bignum (cdr n))))
          (else (cons (- (car n) 1) (cdr n))))))

; for measurements

(define plus
  (lambda (x y)
    (if (is-zero?-bignum x)
        y
        (successor-bignum (plus (predecessor-bignum x) y)))))

(define multiply
  (lambda (x y)
    (if (is-zero?-bignum x)
        (zero-bignum)
        (plus (multiply (predecessor-bignum x) y) y))))

(define factorial
  (lambda (n)
    (if (is-zero?-bignum n)
        (successor-bignum (zero-bignum))
        (multiply n (factorial (predecessor-bignum n))))))

; The execution time for computing the factorial of 10 doesn't vary very much
; when the base N is changed, things get just a tiny bit faster when N grows.
; The reason for this is that the algorithms in plus and multiply are so
; inefficient that making the fundamental operations more or less efficient
; doesn't have much impact.
