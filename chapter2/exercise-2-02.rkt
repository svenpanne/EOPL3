#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.2
;
; The unary representation is quite efficient, but fulfills the specification.
; The representation using Scheme number is only correct iff the underlying
; Scheme numbers support arbitrary precision integers. The bignum representation
; is relatively efficient, especially for larger N, and fulfills the
; specification.
