#lang racket

(require (prefix-in io: "io.rkt"))

(struct table (rows cols shape))
(define (vecvec->table vecvec)
  (define num-rows (vector-length vecvec))
  (define num-cols (vector-length (vector-ref vecvec 0)))
  (define rows (for/hash ([i (in-naturals)] [vec vecvec])
                 (values i vec)))
  (define cols (for/hash ([j (in-range num-cols)])
                 (values j (for/vector ([i (in-range num-rows)])
                             (vector-ref (vector-ref vecvec i) j)))))
  (table rows cols (vector num-rows num-cols)))

(module+ main
  (define S (vecvec->table (io:parse-file "data/train.csv"))))
