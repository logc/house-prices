#lang racket

(require (prefix-in io: "io.rkt")
         "decision.rkt")

(module+ main
  (define S (io:parse-file "data/train.csv")))
