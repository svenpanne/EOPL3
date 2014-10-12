#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.34

(define path
  (lambda (n bst)
    (letrec
        ((loop
          (lambda (bst rev-path)
            (cond ((null? bst) (report-not-found))
                  ((< n (car bst)) (loop (cadr bst) (cons 'left rev-path)))
                  ((> n (car bst)) (loop (caddr bst) (cons 'right rev-path)))
                  (else (reverse rev-path)))))
         (report-not-found
          (lambda ()
            (eopl:error 'path "~s not found in BST ~s.~%" n bst))))
      (loop bst '()))))
