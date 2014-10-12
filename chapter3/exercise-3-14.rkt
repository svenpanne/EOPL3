#lang eopl
; ------------------------------------------------------------------------------
; Exercise 3.14

; See: exercise 3.12 with bool-val, emptylist-val and cons-val removed. After
; that, expval was removed completely to simplify things even more.

; ------------------------------------------------------------------------------
; Scanner and parser specification

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (unary-op ("minus") string)
    (binary-op ((or "+" "-" "*" "/")) string)
    (unary-bool-op ("zero?") string)
    (binary-bool-op ((or "equal?" "greater?" "less?")) string)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (unary-op "(" expression ")") unary-exp)
    (expression (binary-op "(" expression "," expression ")") binary-exp)
    (expression ("if" bool-exp "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno bool-exp "==>" expression) "end") cond-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (bool-exp (unary-bool-op "(" expression ")") unary-bool-exp)
    (bool-exp (binary-bool-op "(" expression "," expression ")") binary-bool-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

; ------------------------------------------------------------------------------
; Debugging helpers for scanner and parser

(define list-the-datatypes
  (lambda()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; ------------------------------------------------------------------------------
; Environments

; See: exercise 2.5

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))

(define init-env
  (lambda ()
    (extend-env 'i 1
      (extend-env 'v 5
        (extend-env 'x 10
          (empty-env))))))

; ------------------------------------------------------------------------------
; Interpreter

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) num)
      (unary-exp (op exp1)
        (let ((val1 (value-of exp1 env)))
          ((unary-table op) val1)))
      (binary-exp (op exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          ((binary-table op) val1 val2)))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of-bool-exp exp1 env)))
          (if val1
              (value-of exp2 env)
              (value-of exp3 env))))
      (cond-exp (lhss rhss)
        (value-of-cond lhss rhss env))
      (var-exp (var)
        (apply-env env var))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env)))))))

(define value-of-cond
  (lambda (lhss rhss env)
    (cond ((null? lhss)
           (eopl:error 'value-of-cond "No LHS evaluated to true"))
          ((value-of-bool-exp (car lhss) env)
           (value-of (car rhss) env))
          (else
           (value-of-cond (cdr lhss) (cdr rhss) env)))))

(define value-of-bool-exp
  (lambda (bexp env)
    (cases bool-exp bexp
      (unary-bool-exp (op exp1)
        (let ((val1 (value-of exp1 env)))
          ((unary-bool-table op) val1)))
      (binary-bool-exp (op exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          ((binary-bool-table op) val1 val2))))))

; ------------------------------------------------------------------------------
; A table for n-ary operations, represented as an association list.

(define make-table
  (lambda lst
    (lambda (op)
      (cdr (assoc op lst)))))

(define entry cons)

; ------------------------------------------------------------------------------
; Tables of n-ary operations.
  
(define unary-table
  (make-table (entry "minus" -)))

(define binary-table
  (make-table (entry "+" +)
              (entry "-" -)
              (entry "*" *)
              (entry "/" quotient)))

(define unary-bool-table
  (make-table (entry "zero?" zero?)))

(define binary-bool-table
  (make-table (entry "equal?" =)
              (entry "greater?" >)
              (entry "less?" <)))

; ------------------------------------------------------------------------------
; A nice REPL for interactive use

(define read-eval-print
  (sllgen:make-rep-loop "-->" value-of-program
    (sllgen:make-stream-parser scanner-spec grammar)))
