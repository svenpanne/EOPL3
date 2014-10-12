#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.30

(define sort/predicate
  (lambda (pred loi)
    (let ((half (quotient (length loi) 2)))
      (if (zero? half)
          loi
          (merge/predicate pred
                           (sort/predicate pred (list-head loi half))
                           (sort/predicate pred (list-tail loi half)))))))

; See: exercise 1.29
(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst) (list-head (cdr lst) (- n 1))))))

(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((pred (car loi1) (car loi2))
           (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
          (else
           (cons (car loi2) (merge/predicate pred loi1 (cdr loi2)))))))
