#lang eopl
; ------------------------------------------------------------------------------
; Exercise B.3

(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (add-op ((or "+" "-")) symbol)
    (mul-op ((or "*" "/")) symbol)))

; We use a separate production for a line of input to make the repl work smooth.
(define grammar
  '((line (arith-expr ";") a-line)
    (arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-arith-factor)
    (arith-factor ("(" arith-expr ")") paren-arith-factor)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define value-of-line
  (lambda (l)
    (cases line l
      (a-line (expr)
        (value-of-arith-expr expr)))))

(define value-of-arith-expr
  (lambda (expr)
    (cases arith-expr expr
      (an-arith-expr (term ops terms)
        (combining-value-of value-of-term term ops terms)))))

(define value-of-term
  (lambda (term)
    (cases arith-term term
      (an-arith-term (factor ops factors)
        (combining-value-of value-of-factor factor ops factors)))))

(define value-of-factor
  (lambda (factor)
    (cases arith-factor factor
      (number-arith-factor (num)
        num)
      (paren-arith-factor (expr)
        (value-of-arith-expr expr)))))

(define combining-value-of
  (lambda (value-of term ops terms)
    (let ((combine (lambda (op term accu)
                     ((operator->func op) accu (value-of term)))))
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

(define operator->func
  (let ((operator-dict (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /))))
    (lambda (op)
      (cdr (assoc op operator-dict)))))

(define read-eval-print
  (sllgen:make-rep-loop "-->" value-of-line
    (sllgen:make-stream-parser scanner-spec grammar)))
