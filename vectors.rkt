#lang racket/base

(provide var-reduction
         vector-index-where)

(require (only-in math sqr))

(module+ test
  (require rackunit))

(define (vector-empty? v)
  (zero? (vector-length v)))

;; var v: (VectorOf Float) -> Float
;; computes the variance of a vector
;; From Wikipedia EN: Variance:
;; variance of a vector of n equally likely values can be equivalently
;; expressed, without directly referring to the mean, in terms of
;; squared deviations of all points from each other
(define (var v)
  (define (actual-var v)
    (let ([n (vector-length v)])
     (* (/ 1 (sqr n))
        (for/sum ([i (in-range n)])
          (for/sum ([j (in-range i n)])
            (sqr (- (vector-ref v i) (vector-ref v j))))))))
  (cond [(vector-empty? v) 0]
        [else (actual-var v)]))

(module+ test
  (define-binary-check (check-in-tolerance actual expected)
    (< (abs (- actual expected)) 0.001))
  (test-case "Variance"
    ;; expected result computed with `numpy.array([10, 11, 50, 51]).var()`
    (check-in-tolerance (var '#(10 11 50 51)) 400.25)))

(define (vector-index-where v proc)
  (for/vector ([elem v] [idx (in-naturals)] #:when (proc elem)) idx))

(module+ test
  (test-case "Vector index where"
    (let ([v '#(1 2 3 4)])
      (check-equal? (vector-index-where v (lambda (x) (<= x 2))) '#(0 1)))))

(define (vector-refs v idxs)
  (for/vector ([idx idxs]) (vector-ref v idx)))

(module+ test
  (test-case "Vector refs"
    (check-equal? (vector-refs '#(10 20 30 40) '#(0 3))
                  '#(10 40))))

(define (var-reduc v v_true v_false)
  (- (var v) (+ (var v_true) (var v_false))))

(define (var-reduction v w predicate)
  (let ([v_true (vector-refs v (vector-index-where w predicate))]
        [v_false (vector-refs v (vector-index-where w (lambda (x) (not (predicate x)))))])
    (var-reduc v v_true v_false)))

(module+ test
  (let ([v '#(10 11 50 51)]
        [w '#( 1  2  3  4)])
    (test-case "Variance reduction"
      ;; expected results computed with `numpy`
      (check-in-tolerance (var-reduc v '#(10 11 50) '#(51))   53.361)
      (check-in-tolerance (var-reduc v '#(10) '#(11 50 51))   53.361)
      (check-in-tolerance (var-reduc v '#(10 11) '#(50 51))  399.750)
      ;; NOTE: this first case should never happen because we would never use a
      ;; value that is not present in the feature column, but we wanted to show
      ;; that the var-reduction function is symmetric
      (check-in-tolerance (var-reduction v w (lambda (x) (<= x 0)))       0)
      (check-in-tolerance (var-reduction v w (lambda (x) (<= x 1)))  53.361)
      (check-in-tolerance (var-reduction v w (lambda (x) (<= x 2))) 399.750)
      (check-in-tolerance (var-reduction v w (lambda (x) (<= x 3)))  53.361)
      (check-in-tolerance (var-reduction v w (lambda (x) (<= x 4)))       0))))
