#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.3
;
; 1) If r is a representation of some number, (diff r (diff q q)) for any
;    diff-tree q is a representation of the same number.
;
; 2) The solution below includes a few helper functions:

; ---------- diff-tree data type
(define one (lambda () '(one)))

(define diff
  (lambda (n1 n2)
    (list 'diff n1 n2)))

(define tag-diff-tree car)
(define pos-diff-tree cadr)
(define neg-diff-tree caddr)

(define is-one?
  (lambda (n)
    (eqv? (tag-diff-tree (one)) (tag-diff-tree n))))
 
; ---------- utility functions for the diff-tree data type
(define zero-diff-tree
  (lambda ()
    (diff (one) (one))))

(define negate-diff-tree
  (lambda (n)
    (if (is-one? n)
        (diff (zero-diff-tree) (one))
        (diff (neg-diff-tree n) (pos-diff-tree n)))))

; A version not relying on Scheme numbers. We basically evaluate the given
; diff-tree, keeping its value in a normalized form (diff p n). If p is (one),
; the value is positive or 0. If n is (one), the value is negative or 0.
(define is-zero?-diff-tree
  (lambda (n)
    (equal? (zero-diff-tree) (eval-diff-tree n (zero-diff-tree)))))

; [(eval-diff-tree n accu)] = -([n] + [accu]), proof by induction on n:
; 
; 1)   [(eval-diff-tree (one) accu)]
;    = [(predecessor-nat (negate-diff-tree accu))]
;    = -[accu] - 1
;    = -(1 + [accu])
;    = -([(one)] + [accu])   OK
;
; 2)   [(eval-diff-tree (diff p n) accu)]
;    = [(eval-diff-tree p (eval-diff-tree n (negate-diff-tree accu)))]
;    = -([p] + [(eval-diff-tree n (negate-diff-tree accu))])
;    = -([p] + (-([n] + [(negate-diff-tree accu)])))
;    = -([p] - [n] - [(negate-diff-tree accu)])
;    = -(([p] - [n]) + [accu])
;    = -([(diff p n)] + [accu])   OK
(define eval-diff-tree
  (lambda (n accu)
    (if (is-one? n)
        (predecessor-normalized (negate-diff-tree accu))
        (eval-diff-tree (pos-diff-tree n)
                        (eval-diff-tree (neg-diff-tree n) (negate-diff-tree accu))))))

(define predecessor-normalized
  (lambda (n)
    (if (is-one? (neg-diff-tree n))
        (diff n (one)) ; [n] is negative or zero
        (diff (one) (pos-diff-tree (neg-diff-tree n)))))) ; [n] is positive or 0

(define successor-diff-tree
  (lambda (n)
    (diff-tree-plus n (one))))

(define predecessor-diff-tree
  (lambda (n)
    (diff n (one))))

; 3)

(define diff-tree-plus
  (lambda (n1 n2)
    (diff n1 (diff (zero-diff-tree) n2))))
