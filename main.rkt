#lang racket

(require (prefix-in io: "io.rkt"))

(module+ main
  (define M (io:parse-file "data/train.csv")))
