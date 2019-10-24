#lang racket

(require (prefix-in io: "io.rkt")
         (prefix-in tb: "tables.rkt")
         (prefix-in dt: "decision.rkt")
         "decision.rkt")

(module+ main
  (define S  (io:parse-file "data/train.csv"))
  (define T  (tb:vecvec->table S))
  (define DT (dt:train-tree T 0 100)))
