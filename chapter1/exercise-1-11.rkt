#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.11
;
; Termination is guaranteed because recursion is via subst, which calls
; subst-in-sexp with a smaller expression.
