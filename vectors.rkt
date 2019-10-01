#lang racket

(provide vector-first
         vector-rest
         vector-empty?)

(define (vector-indexes-where vec pred?)
  (for/vector
      ([i (in-range (vector-length vec))]
       [elem vec] #:when (pred? elem))
    i))

(module+ test
  (require rackunit)
  (test-case "vector-indexes-where"
    (define example (vector 1 2 3 4))
    (check-equal? (vector-indexes-where example even?) (vector 1 3))
    (check-equal? (vector-indexes-where (vector) even?) (vector))
    (check-equal? (vector-indexes-where example string?) (vector))))

(define (vector-ref-by vec indexes)
  (define in-indexes '())
  (define not-in-indexes '())
  (for ([i (in-range (vector-length vec))]
        [e vec])
    (if (vector-member i indexes)
        (set! in-indexes (append in-indexes (list e)))
        (set! not-in-indexes (append not-in-indexes (list e)))))
  (values (list->vector in-indexes)
          (list->vector not-in-indexes)))

(module+ test
  (test-case "vector-split-by"
    (define example (vector 1 2 3 4))
    (define indexes (vector 0 1))
    (let-values ([(in out) (vector-ref-by example indexes)])
      (check-equal? in (vector 1 2))
      (check-equal? out (vector 3 4)))))

(define (vector-first v)
  (vector-ref v 0))

(define (vector-rest v)
  (for/vector ([indx (in-naturals)]
               [elem v]
               #:when (> indx 0))
    elem))

(define (vector-empty? v)
  (zero? (vector-length v)))

(module+ test
  (test-case "vector first and rest"
    (let ([v (vector 1 2 3 4)])
      (check-equal? (vector-first v) 1)
      (check-equal? (vector-rest  v) (vector 2 3 4)))))
