#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.18

(define number->sequence (lambda (n) (make-sequence n '() '())))

(define make-sequence
  (lambda (current-element left-part right-part)
    (list current-element left-part right-part)))

(define current-element car)
(define left-part cadr)
(define right-part caddr)

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        (report-moving-too-far 'move-to-left)
        (make-sequence (car (left-part seq))
                       (cdr (left-part seq))
                       (cons (current-element seq) (right-part seq))))))

(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        (report-moving-too-far 'move-to-right)
        (make-sequence (car (right-part seq))
                       (cons (current-element seq) (left-part seq))
                       (cdr (right-part seq))))))

(define insert-to-left
  (lambda (n seq)
    (make-sequence (current-element seq)
                   (cons n (left-part seq))
                   (right-part seq))))

(define insert-to-right
  (lambda (n seq)
    (make-sequence (current-element seq)
                   (left-part seq)
                   (cons n (right-part seq)))))

(define at-left-end? (lambda (seq) (null? (left-part seq))))
(define at-right-end? (lambda (seq) (null? (right-part seq))))

(define report-moving-too-far
  (lambda (func)
    (eopl:error func "Moved too far")))
