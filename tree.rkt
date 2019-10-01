#lang racket

(module+ test
  (require rackunit))

(struct tree (predicate left right))
(struct leaf (nodes))

;; var X: (VectorOf Float) -> Float
;; computes the variance of a vector
;; From Wikipedia EN: Variance:
;; variance of a vector of n equally likely values can be equivalently
;; expressed, without directly referring to the mean, in terms of
;; squared deviations of all points from each other
(define (var X)
  (let ([n (vector-length X)])
    (* (/ 1 (sqr n))
       (for/sum ([i (in-range n)])
         (for/sum ([j (in-range i n)])
           (sqr (- (vector-ref X i) (vector-ref X j))))))))
