#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.35

; let-values and friends are not part of #eopl, so we simply use a pair.
(define number-leaves
  (lambda (b)
    (letrec ((loop
              (lambda (b n)
                (if (leaf? b)
                    (cons n (+ n 1))
                    (let* ((bnl (loop (lson b) n))
                           (bnr (loop (rson b) (cdr bnl))))
                      (cons (interior-node (contents-of b) (car bnl) (car bnr))
                            (cdr bnr)))))))
      (car (loop b 0)))))

; See: exercise 1.31
(define leaf (lambda (i) i))
(define interior-node (lambda (s i1 i2) (list s i1 i2)))
(define leaf? integer?)
(define lson cadr)
(define rson caddr)
(define contents-of (lambda (b) (if (leaf? b) b (car b))))
