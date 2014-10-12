#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.5
;
; 1) e has the form Identifier:
;    There are 0 parentheses, therefore they are balanced.
; 2a) e has the form (lambda (Identifier) LcExp):
;     If LcExp has n opening/closing parentheses, e has n+2 opening/closing
;     parentheses, therefore they are balanced
; 2b) e has the form (LcExp LcExp):
;     If the first LcExp has n opening/closing parentheses and the second one m,
;     e has n+m+1 opening/closing parentheses.
