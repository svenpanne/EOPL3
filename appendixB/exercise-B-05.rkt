#lang eopl
; ------------------------------------------------------------------------------
; Exercise B.5

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

; See: exercise B.4
; Note that we use the grammar for operators instead of the scanner spec now,
; otherwise the scanner always emits add-op for "-", even for unary minus.
(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)))

; We use a separate production for a line of input to make the repl work smooth.
(define grammar
  '((line (arith-expr ";") a-line)
    (arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-arith-factor)
    (arith-factor (identifier) var-arith-factor)
    (arith-factor ("(" arith-expr ")") paren-arith-factor)
    (arith-factor ("-" arith-factor) unary-minus-arith-factor)
    (add-op ("+") plus-add-op)
    (add-op ("-") minus-add-op)
    (mul-op ("*") mul-mul-op)
    (mul-op ("/") div-mul-op)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define value-of-line
  (lambda (env)
    (lambda (l)
      (cases line l
        (a-line (expr)
          (value-of-arith-expr env expr))))))

(define value-of-arith-expr
  (lambda (env expr)
    (cases arith-expr expr
      (an-arith-expr (term ops terms)
        (combining-value-of (value-of-term env) term (map value-of-add-op ops) terms)))))

(define value-of-term
  (lambda (env)
    (lambda (term)
      (cases arith-term term
        (an-arith-term (factor ops factors)
          (combining-value-of (value-of-factor env) factor (map value-of-mul-op ops) factors))))))

(define value-of-factor
  (lambda (env)
    (lambda (factor)
      (cases arith-factor factor
        (number-arith-factor (num)
          num)
        (var-arith-factor (var)
          (apply-env env var))
        (paren-arith-factor (expr)
          (value-of-arith-expr env expr))
        (unary-minus-arith-factor (factor)
           (- ((value-of-factor env) factor)))))))

(define value-of-add-op
  (lambda (op)
    (cases add-op op
      (plus-add-op () +)
      (minus-add-op () -))))

(define value-of-mul-op
  (lambda (op)
    (cases mul-op op
      (mul-mul-op () *)
      (div-mul-op () /))))

(define combining-value-of
  (lambda (value-of term ops terms)
    (let ((combine (lambda (op term accu)
                     (op accu (value-of term)))))
      (foldl-2 combine (value-of term) ops terms))))

; No 'foldl' in #lang eopl, use a specialized version for 2 lists.
(define foldl-2
  (lambda (func init lst1 lst2)
    (if (null? lst1)
        init
        (foldl-2 func
                 (func (car lst1) (car lst2) init)
                 (cdr lst1)
                 (cdr lst2)))))

(define example-env
  (extend-env 'foo 123
    (extend-env 'bar 456
      (empty-env))))

(define read-eval-print
  (let ((parser (sllgen:make-stream-parser scanner-spec grammar)))
    (lambda (env)
      ((sllgen:make-rep-loop "-->" (value-of-line env) parser)))))
