#lang racket

(require (prefix-in io: "io.rkt")
         "vectors.rkt")

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
(define (table->vecvec a-table)
  (for/vector ([i (in-range (table-num-rows a-table))])
    (hash-ref (table-rows a-table) i)))
(define (table-num-rows a-table)
  (vector-ref (table-shape a-table) 0))
(define (table-num-cols a-table)
  (vector-ref (table-shape a-table) 1))
(define (get-column a-table col-index)
  (hash-ref (table-cols a-table) col-index))
(define (get-row a-table row-index)
  (hash-ref (table-rows a-table) row-index))
(define (get-labels a-table)
  ;; assumes that last column always holds labels
  (hash-ref (table-cols a-table) (sub1 (table-num-cols a-table))))

(module+ test
  (require rackunit)
  (test-case "Table"
    (define vecvec (vector (vector 1 2) (vector 3 4) (vector 5 6)))
    (define a-table (vecvec->table vecvec))
    (check-eq? (table-num-rows a-table) 3)
    (check-eq? (table-num-cols a-table) 2)
    (check-equal? (get-column a-table 0) (vector 1 3 5))
    (check-equal? (get-column a-table 1) (vector 2 4 6))
    (check-equal? (get-row a-table 0) (vector 1 2))
    (check-equal? (get-labels a-table) (vector 2 4 6))
    (check-equal? (table->vecvec a-table) vecvec)))


(struct tree (predicate left right))
(struct leaf (nodes))

(define (predict a-tree a-node)
  (let ([predicate (tree-predicate a-tree)])
    (cond
      [(and (leaf? (tree-left a-tree))
            (not (leaf? (tree-right a-tree))))
       (if (predicate a-node)
           (leaf-nodes (tree-left a-tree))
           (predict (tree-right a-tree) a-node))]
      [(and (not (leaf? (tree-left a-tree)))
            (leaf? (tree-right a-tree)))
       (if (predicate a-node)
           (predict (tree-left a-tree) a-node)
           (leaf-nodes (tree-right a-tree)))]
      [(and (leaf? (tree-left a-tree))
            (leaf? (tree-right a-tree)))
       (if (predicate a-node)
           (leaf-nodes (tree-left a-tree))
           (leaf-nodes (tree-right a-tree)))]
      [else
       (if (predicate a-node)
           (predict (tree-left a-tree)  a-node)
           (predict (tree-right a-tree) a-node))])))

(module+ test
  (test-case "Predict"
    (let ([less-than-two (lambda (x) (< x 2))]
          [one-two-three-leaf (leaf '(1 2 3))]
          [four-five-six-leaf (leaf '(4 5 6))])
      (let ([a-tree (tree less-than-two one-two-three-leaf four-five-six-leaf)])
        (check-equal? (predict a-tree 1)  '(1 2 3))
        (check-equal? (predict a-tree 2)  '(4 5 6))
        (check-equal? (predict a-tree 17) '(4 5 6))))
    (let ([less-than-ten    (lambda (x) (< x 10))]
          [less-than-twenty (lambda (x) (< x 20))]
          [less-than-thirty (lambda (x) (< x 30))]
          [one-two-leaf     (leaf '(1 2))]
          [three-four-leaf  (leaf '(3 4))]
          [five-six-leaf    (leaf '(5 6))]
          [seven-eight-leaf (leaf '(7 8))])
      (let ([a-subtree (tree less-than-ten one-two-leaf three-four-leaf)]
            [b-subtree (tree less-than-twenty five-six-leaf seven-eight-leaf)])
        (let ([a-tree (tree less-than-thirty a-subtree b-subtree)])
          (check-equal? (predict a-tree  1) '(1 2))
          (check-equal? (predict a-tree 31) '(7 8))
          (check-equal? (predict a-tree 21) '(3 4)))
        ;; root
        ;;     |-> a-subtree
        ;;             |-> (1 2)
        ;;             |-> (3 4)
        ;;     |-> (5 6)
        (let ([b-tree (tree less-than-thirty a-subtree five-six-leaf)])
          (check-equal? (predict b-tree 31) '(5 6))
          (check-equal? (predict b-tree 11) '(3 4))
          (check-equal? (predict b-tree  9) '(1 2)))
        ;; root
        ;;     |-> (1 2)
        ;;     |-> b-subtree
        ;;             |-> (5 6)
        ;;             |-> (7 8)
        (let ([c-tree (tree less-than-thirty one-two-leaf b-subtree)])
          (check-equal? (predict c-tree 1) '(1 2))
          (check-equal? (predict c-tree 31) '(7 8)))))))

(define (make-subtree column labels)
  ;; TODO implement stub
  (tree 'whatever (vector 10 20) (vector 30)))

(module+ test
  (test-case "Make subtree"
    (let ([a-column (vector 10  20  30)]
          [b-column (vector 100 200 300)]
          [labels   (vector 10  11  90)])
      (let ([a-subtree (make-subtree a-column labels)]
            [b-subtree (make-subtree b-column labels)])
        (check-equal? (tree-left a-subtree) (vector 10 20))
        (check-equal? (tree-left b-subtree) (vector 100 200))))))

(define (split-labels-by-value column labels value)
  (define (split-recur col lab val smaller larger)
    (cond [(vector-empty? col)
           (values (list->vector smaller)
                   (list->vector larger))]
          [else
           (if (<= (vector-first col) val)
               (split-recur (vector-rest col)
                            (vector-rest lab)
                            val
                            (append smaller (list (vector-first lab)))
                            larger)
               (split-recur (vector-rest col)
                            (vector-rest lab)
                            val
                            smaller
                            (append larger (list (vector-first lab)))))]))
  (split-recur column labels value '() '()))

(module+ test
  (test-case "Split labels by column value"
    (let ([column (vector 10 20 30)]
          [labels (vector 11 24 39)])
      (let-values
          ([(smaller larger)
            (split-labels-by-value column labels (vector-ref column 0))])
        (check-equal? smaller (vector 11))
        (check-equal? larger  (vector 24 39))))))

(define (table->tree a-table)
  (let ([labels (get-labels a-table)])
    (for ([col-idx (in-range (table-num-cols a-table))])
      (let ([column (get-column a-table col-idx)])
        (for ([row-idx (in-range (table-num-cols a-table))])
          (let ([value (vector-ref column row-idx)])
            (for ([other-row-idx (in-range (table-num-cols a-table))]
                  #:unless (= row-idx other-row-idx))
              (let ([other-value (vector-ref column other-row-idx)])
                ;; TODO: continue here
                '()

                ))))))))

(module+ main
  (define S (vecvec->table (io:parse-file "data/train.csv"))))
