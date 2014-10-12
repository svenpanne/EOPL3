#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.29

(define sort
  (lambda (loi)
    (let ((half (quotient (length loi) 2)))
      (if (zero? half)
          loi
          (merge (sort (list-head loi half))
                 (sort (list-tail loi half)))))))

(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst) (list-head (cdr lst) (- n 1))))))

; See: exercise 1.28
(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((< (car loi1) (car loi2))
           (cons (car loi1) (merge (cdr loi1) loi2)))
          (else
           (cons (car loi2) (merge loi1 (cdr loi2)))))))
