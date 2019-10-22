#lang racket/base

;; Predicates & decisions, i.e. how and when to split a branch out
(provide decision-max)

(require (only-in racket/list empty? argmax))

(require "vectors.rkt")

(module+ test
  (require rackunit))

(define (predicate-max feature labels)
  (define reduction-predicate-pairs
    (for/list ([value feature])
      (let ([predicate (lambda (x) (<= x value))])
        (cons (var-reduction labels feature predicate) predicate))))
  (argmax car reduction-predicate-pairs))

(module+ test
  (test-case "Predicate max"
    (let ([labels  '#(10 11 50 51) ]
          [feature '#( 1  2  3  4)])
      (let ([reduction-predicate-pair (predicate-max feature labels)])
        (let ([predicate (cdr reduction-predicate-pair)])
          (check-true  (predicate 1))
          (check-true  (predicate 2))
          (check-false (predicate 3))
          (check-false (predicate 4)))))))

;; a "decision" is a pair of "selected feature" and "comparison predicate"
(define (decision-max features labels)
  (define reduction-decision-pairs
    (for/list ([feature features] [idx (in-naturals)])
      (let ([reduction-predicate-pair (predicate-max feature labels)])
        (let ([reduction (car reduction-predicate-pair)]
              [predicate (cdr reduction-predicate-pair)])
          (let ([decision
                 (lambda (sample)
                   (let ([f (vector-ref sample idx)])
                     (predicate f)))])
            (cons reduction decision))))))
  (argmax car reduction-decision-pairs))

(module+ test
  (test-case "Decision max"
    (let ([labels    '#(10 11 50 51)]
          [a_feature '#( 1  2  3  4)]
          [b_feature '#( 5  4  3  2)])
      (let ([reduction-decision-pair (decision-max (list a_feature b_feature) labels)])
        (let ([decision (cdr reduction-decision-pair)])
          ;; rows are taken from a_feature, then b_feature, in order
          (let ([a_row '#(1 5)]
                [b_row '#(2 4)]
                [c_row '#(3 3)]
                [d_row '#(4 2)])
            ;; the results must be the same as the predicate-max example because
            ;; the selected feature should be a_feature, and the predicate is
            ;; the same as for the predicate-max test
            (check-true (decision a_row))
            (check-true (decision b_row))
            (check-false (decision c_row))
            (check-false (decision d_row))))))))
