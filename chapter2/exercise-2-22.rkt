#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.22

(define-datatype stack stack?
  (empty-stack)
  (push
   (saved-val scheme-value?)
   (saved-stack stack?)))

(define scheme-value? (lambda (s) #t))

(define pop
  (lambda (s)
    (cases stack s
      (empty-stack ()
                   (report-empty-stack 'pop))
      (push (saved-val saved-stack)
            saved-stack))))

(define report-empty-stack
  (lambda (func)
    (eopl:error func "Empty stack")))

(define top
  (lambda (s)
    (cases stack s
      (empty-stack ()
                   (report-empty-stack 'top))
      (push (saved-val saved-stack)
            saved-val))))

(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack ()
                   #t)
      (push (saved-val saved-stack)
            #f))))
