#lang racket/base

(require (only-in math sqr)
         (only-in racket/list empty? argmax)
         (only-in racket/vector vector-filter))

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

;; Tables
;; TODO: refactor into its own module
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
(define (get-rows a-table)
  (for/vector ([idx (in-range (table-num-rows a-table))])
    (get-row a-table idx)))
(define (get-features a-table)
  (for/list ([i (in-range (table-num-cols a-table))]
             #:unless (= i (sub1 (table-num-cols a-table))))
    (hash-ref (table-cols a-table) i)))
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
    (check-equal? (get-features a-table) (list (vector 1 3 5)))
    (check-equal? (get-labels a-table) (vector 2 4 6))
    (check-equal? (table->vecvec a-table) vecvec)))

;; Decision trees
;; TODO: refactor into its own module
(struct tree (decision left right) #:transparent)
(struct leaf (nodes) #:transparent)

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

(define (train-tree a-table [depth 0] [minimum-rows 2])
  (cond [(<= (table-num-rows a-table) minimum-rows) (leaf (table->vecvec a-table))]
        [else
         (let ([features (get-features a-table)]
               [labels   (get-labels   a-table)])
           (let ([reduction-decision-pair (decision-max features labels)])
             (let ([decision (cdr reduction-decision-pair)])
               (let ([rows-true (vector-filter decision (get-rows a-table))]
                     [rows-false (vector-filter (lambda (row) (not (decision row))) (get-rows a-table))])
                 (tree decision
                       (train-tree (vecvec->table rows-true) (add1 depth))
                       (train-tree (vecvec->table rows-false) (add1 depth)))))))]))

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
