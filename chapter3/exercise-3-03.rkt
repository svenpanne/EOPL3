#lang eopl
; ------------------------------------------------------------------------------
; Exercise 3.3

; One can emulate addition via subtraction (but not the other way round):
;   a + b = a - (-b) = a - (0 - b) = a - ((c - c) - b)
