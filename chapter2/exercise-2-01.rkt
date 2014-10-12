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
          ((= (+ (car n) 1) N) (cons 0 (successor-bignum (cdr n))))
          (else (cons (+ (car n) 1) (cdr n))))))

(define predecessor-bignum
  (lambda (n)
    (cond ((null? n) (eopl:error 'predecessor-bignum "zero.~%"))
          ((zero? (car n)) (cons (- N 1) (predecessor-bignum (cdr n))))
          ((and (= (car n) 1) (null? (cdr n))) '())
          (else (cons (- (car n) 1) (cdr n))))))
