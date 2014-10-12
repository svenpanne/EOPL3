#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.4
;
;    List-of-Int
; => (Int . List-of-Int)
; => (-7 . List-of-Int)
; => (-7 . (Int . List-of-Int))
; => (-7 . (3 . List-of-Int))
; => (-7 . (3 . (Int . List-of-Int)))
; => (-7 . (3 . (14 . List-of-Int)))
; => (-7 . (3 . (14 . ())))
