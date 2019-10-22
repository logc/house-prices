#lang racket/base

(provide train-tree
         predict
         prediction->regression)

(require (only-in math sqr)
         (only-in racket/vector vector-filter))

(require "splits.rkt"
         "tables.rkt")

(module+ test
  (require rackunit))

(struct tree (decision left right))
(struct leaf (nodes))

(define (predict a-tree a-node)
  (let ([decision (tree-decision a-tree)])
    (cond
      [(and (leaf? (tree-left a-tree))
            (not (leaf? (tree-right a-tree))))
       (if (decision a-node)
           (leaf-nodes (tree-left a-tree))
           (predict (tree-right a-tree) a-node))]
      [(and (not (leaf? (tree-left a-tree)))
            (leaf? (tree-right a-tree)))
       (if (decision a-node)
           (predict (tree-left a-tree) a-node)
           (leaf-nodes (tree-right a-tree)))]
      [(and (leaf? (tree-left a-tree))
            (leaf? (tree-right a-tree)))
       (if (decision a-node)
           (leaf-nodes (tree-left a-tree))
           (leaf-nodes (tree-right a-tree)))]
      [else
       (if (decision a-node)
           (predict (tree-left a-tree)  a-node)
           (predict (tree-right a-tree) a-node))])))

(define (prediction->regression prediction-nodes)
  (let ([N (vector-length prediction-nodes)]
        [last-idx (sub1 (vector-length (vector-ref prediction-nodes 0)))])
    (/ (for/sum ([n prediction-nodes]) (vector-ref n last-idx))
       N)))

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

(define (train-tree a-table [depth 0] [minimum-rows 2] [maximum-depth 100])
  (cond [(or (<= (table-num-rows a-table) minimum-rows)
             (>= depth maximum-depth))
         (leaf (table->vecvec a-table))]
        [else
         (let ([features (get-features a-table)]
               [labels   (get-labels   a-table)])
           (let ([reduction-decision-pair (decision-max features labels)])
             (let ([decision (cdr reduction-decision-pair)])
               (let ([rows-true (vector-filter decision (get-rows a-table))]
                     [rows-false (vector-filter (lambda (row) (not (decision row))) (get-rows a-table))])
                 (tree decision
                       (train-tree (vecvec->table rows-true) (add1 depth) minimum-rows maximum-depth)
                       (train-tree (vecvec->table rows-false) (add1 depth) minimum-rows maximum-depth))))))]))

(module+ test
  (test-case "Train a tree"
    (let ([vecvec '#(#(1 5 10)
                     #(2 5 11)
                     #(3 5 50)
                     #(4 5 51))])
      ;; we expect the tree trained out of this data to have a single decision split,
      ;; where the first two samples go to the left and the second two samples go to
      ;; the right branch. The decision depends only on the first feature, splitting
      ;; at <= 2, and the regression values predicted are either 10.5 or 50.5
      (let ([a-tree (train-tree (vecvec->table vecvec))])
        (check-eqv? (prediction->regression (predict a-tree '#(1 5))) 21/2)
        (check-eqv? (prediction->regression (predict a-tree '#(2 5))) 21/2)
        (check-eqv? (prediction->regression (predict a-tree '#(3 5))) 101/2)
        (check-eqv? (prediction->regression (predict a-tree '#(4 5))) 101/2)
        ;; the second feature value does not affect the prediction at all
        (check-eqv? (prediction->regression (predict a-tree '#(1 55))) 21/2)))
    (let ([vecvec '#(#(1 5 10)
                     #(2 5 11)
                     #(3 5 50)
                     #(4 5 51)
                     ;; we expect these two new samples to land on their own branch
                     ;; of the tree, which is split based on the second feature
                     #(4 20 1000)
                     #(4 21 1001))])
      (let ([a-tree (train-tree (vecvec->table vecvec))])
        ;; the expected values are the same as the predictions from a
        ;; `sklearn.tree.DecisionTreeRegressor(random_state=0)` fitted to same data
        (check-eqv? (prediction->regression (predict a-tree '#(1 5))) 21/2)
        (check-eqv? (prediction->regression (predict a-tree '#(3 4))) 101/2)
        (check-eqv? (prediction->regression (predict a-tree '#(1 30))) 2001/2)))))
