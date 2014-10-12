#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.28

; See: exercise 2.23
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define identifier?
  (lambda (x)
    (and (symbol? x) (not (eqv? 'lambda x)))))

; See: section 2.5 of the EOPL book, just for testing
(define parse-expression
  (lambda (datum)
    (cond ((symbol? datum)
           (var-exp datum))
          ((pair? datum)
           (if (eqv? (car datum) 'lambda)
               (lambda-exp (car (cadr datum))
                           (parse-expression (caddr datum)))
               (app-exp (parse-expression (car datum))
                        (parse-expression (cadr datum)))))
          (else
           (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression "Syntax error: ~s" datum)))

; String ports are not available with #lang eopl, so we do it directly.
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var)
        (symbol->string var))
      (lambda-exp (bound-var body)
        (string-append "(lambda (" (symbol->string bound-var) ") "
                       (unparse-lc-exp body) ")"))
      (app-exp (rator rand)
        (string-append "(" (unparse-lc-exp rator) " "
                       (unparse-lc-exp rand) ")")))))
