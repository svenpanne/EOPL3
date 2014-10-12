#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.14
;
; 1) Induction base n = 0: partial-vector-sum returns (vector-ref v 0) = v(0),
;    which exists because 0 < length(v), and sum from i = 0 to 0 of v(i) = v(0).
; 2) Induction step n > 0: Assume that partial-vector-sum is correct for n.
;    partial-vector-sum returns v(n) + sum from i = 0 to n-1 of v(i), which
;    equals sum from i = 0 to n of v(i).
