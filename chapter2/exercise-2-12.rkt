#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.12

; A variant using a triple of functions.
(define make-stack
  (lambda (pop-func top-func empty-stack?-func)
    (list pop-func top-func empty-stack?-func)))

(define pop-func car)
(define top-func cadr)
(define empty-stack?-func caddr)

(define empty-stack
  (lambda ()
    (make-stack
     (lambda () (report-empty-stack 'pop))
     (lambda () (report-empty-stack 'top))
     (lambda () #t))))

(define push
  (lambda (stack value)
    (make-stack
     (lambda () stack)
     (lambda () value)
     (lambda () #f))))

(define pop
  (lambda (stack)
    ((pop-func stack))))

(define top
  (lambda (stack)
    ((top-func stack))))

(define empty-stack?
  (lambda (stack)
    ((empty-stack?-func stack))))

(define report-empty-stack
  (lambda (func)
    (eopl:error func "Empty stack")))

; A variant using a more "OO"-like interface.
(define empty-stack-oo
  (lambda ()
    (letrec ((this (lambda (message . args)
                     (case message
                       ((push) (push-oo this (car args)))
                       ((pop) (report-empty-stack 'pop))
                       ((top) (report-empty-stack 'top))
                       ((empty-stack?) #t)))))
      this)))

(define push-oo
  (lambda (stack value)
    (letrec ((this (lambda (message . args)
                     (case message
                       ((push) (push-oo this (car args)))
                       ((pop) stack)
                       ((top) value)
                       ((empty-stack?) #f)))))
      this)))
