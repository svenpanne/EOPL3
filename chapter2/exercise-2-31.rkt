#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.31

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define-datatype parse-result parse-result?
  (a-parse-result
   (exp prefix-exp?)
   (rest list?)))

(define parse-expression
  (lambda (prefix-list)
    (cases parse-result (parse-prefix-exps prefix-list)
      (a-parse-result (exp rest)
        (if (null? rest)
            exp
            (eopl:error 'parse-expression "Unused rest: ~s" rest))))))

(define parse-prefix-exps
  (lambda (prefix-exps)
    (if (pair? prefix-exps)
        (let ((prefix-exp (car prefix-exps))
              (rest (cdr prefix-exps)))
          (cond ((integer? prefix-exp)
                 (a-parse-result (const-exp prefix-exp) rest))
                ((eqv? prefix-exp '-)
                 (cases parse-result (parse-prefix-exps rest)
                   (a-parse-result (exp1 rest1)
                     (cases parse-result (parse-prefix-exps rest1)
                       (a-parse-result (exp2 rest2)
                         (a-parse-result (diff-exp exp1 exp2) rest2))))))
                (else
                 (eopl:error 'parse-expression
                             "Expected integer or -, got ~s" prefix-exp))))
        (eopl:error 'parse-expression "Not a prefix list: ~s" prefix-exps))))
