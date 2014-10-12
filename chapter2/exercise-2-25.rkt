#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.25

; See: exercise 2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define-datatype result result?
  (a-result
   (sum-result integer?)
   (best-result best?)))

(define sum-result
  (lambda (r)
    (cases result r
      (a-result (sum best)
        sum))))

(define best-result
  (lambda (r)
    (cases result r
      (a-result (sum best)
        best))))

(define-datatype best best?
  (a-best
   (max-best integer?)
   (key-best symbol?)))

(define max-best
  (lambda (b)
    (cases best b
      (a-best (max key)
        max))))

(define key-best
  (lambda (b)
    (cases best b
      (a-best (max key)
        key))))

(define max-interior
  (lambda (bt)
    (key-best (best-result (max-interior-helper bt)))))

(define max-interior-helper
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num)
        (eopl:error 'max-interior "Empty tree"))
      (interior-node (key left right)
        (cases bintree left
          (leaf-node (num-l)
            (cases bintree right
              (leaf-node (num-r)
                (let ((sum (+ num-l num-r)))
                  (a-result sum (a-best sum key))))
              (interior-node (key-r left-r right-r)
                (let* ((result-r (max-interior-helper right))
                       (sum (+ num-l (sum-result result-r))))
                  (a-result sum (choose-maximum (a-best sum key)
                                                (best-result result-r)))))))
          (interior-node (key-l left-l right-l)
            (cases bintree right
              (leaf-node (num-r)
                (let* ((result-l (max-interior-helper left))
                       (sum (+ (sum-result result-l) num-r)))
                  (a-result sum (choose-maximum (a-best sum key)
                                                (best-result result-l)))))
              (interior-node (key-r left-r right-r)
                (let* ((result-l (max-interior-helper left))
                       (result-r (max-interior-helper right))
                       (sum (+ (sum-result result-l) (sum-result result-r))))
                  (a-result sum
                            (choose-maximum
                             (a-best sum key)
                             (choose-maximum (best-result result-l)
                                             (best-result result-r)))))))))))))

(define choose-maximum
  (lambda (best1 best2)
    (if (> (max-best best1) (max-best best2)) best1 best2)))

(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))
