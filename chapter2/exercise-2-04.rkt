#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.4
;
; (empty-stack)            = |[]|
; (push v |[v1 v2 v3...]|) = |[v v1 v2 v3...]|
; (pop |[v1 v2 v3...]|)    = |[v2 v3...]|
; (top |[v1 v2 v3...]|)    = v1
; (empty-stack? s)         = #t if s = |[]|, else #f
;
; constructors: empty-stack, push
; observers: pop, top, empty-stack?
