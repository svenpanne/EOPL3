#lang eopl
; ------------------------------------------------------------------------------
; Exercise B.1

(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (add-op ((or "+" "-")) symbol)
    (mul-op ((or "*" "/")) symbol)))

(define grammar
  '((arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-arith-factor)
    (arith-factor ("(" arith-expr ")") paren-arith-factor)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))
