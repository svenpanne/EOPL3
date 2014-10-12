#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.27

((lambda (a) (a b)) c)

;                app-exp
;                /     \
;       lambda-exp     var-exp
;        /      \         |
;  bound-var    body      c
;      |         |
;      a      app-exp
;             /     \
;        var-exp   var-exp
;           |          |
;           a          b

(lambda (x)
  (lambda (y)
    ((lambda (x)
       (x y))
     x)))

;       lambda-exp
;        /       \
;  bound-var    body
;      |         |
;      x     lambda-exp
;             /      \
;       bound-var    body
;           |         |
;           y      app-exp
;                  /     \
;         lambda-exp     var-exp
;          /      \         |
;    bound-var    body      x
;        |         |
;        x      app-exp
;               /     \
;          var-exp   var-exp
;             |          |
;             x          y
