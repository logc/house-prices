#lang racket

(provide parse-file)

(module+ test
  (require rackunit)
  (define sample-line "1,60,RL,65,8450,Pave,NA,Reg,Lvl,AllPub,Inside,Gtl,CollgCr,Norm,Norm,1Fam,2Story,7,5,2003,2003,Gable,CompShg,VinylSd,VinylSd,BrkFace,196,Gd,TA,PConc,Gd,TA,No,GLQ,706,Unf,0,150,856,GasA,Ex,Y,SBrkr,856,854,0,1710,1,0,2,1,3,1,Gd,8,Typ,0,NA,Attchd,2003,RFn,2,548,TA,TA,Y,0,61,0,0,0,0,NA,NA,NA,0,2,2008,WD,Normal,208500")
  (define sample-file "Id,MSSubClass,MSZoning,LotFrontage,LotArea,Street,Alley,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,YearRemodAdd,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtFinType2,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,Heating,HeatingQC,CentralAir,Electrical,1stFlrSF,2ndFlrSF,LowQualFinSF,GrLivArea,BsmtFullBath,BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,Functional,Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,3SsnPorch,ScreenPorch,PoolArea,PoolQC,Fence,MiscFeature,MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice
1,60,RL,65,8450,Pave,NA,Reg,Lvl,AllPub,Inside,Gtl,CollgCr,Norm,Norm,1Fam,2Story,7,5,2003,2003,Gable,CompShg,VinylSd,VinylSd,BrkFace,196,Gd,TA,PConc,Gd,TA,No,GLQ,706,Unf,0,150,856,GasA,Ex,Y,SBrkr,856,854,0,1710,1,0,2,1,3,1,Gd,8,Typ,0,NA,Attchd,2003,RFn,2,548,TA,TA,Y,0,61,0,0,0,0,NA,NA,NA,0,2,2008,WD,Normal,208500
2,20,RL,80,9600,Pave,NA,Reg,Lvl,AllPub,FR2,Gtl,Veenker,Feedr,Norm,1Fam,1Story,6,8,1976,1976,Gable,CompShg,MetalSd,MetalSd,None,0,TA,TA,CBlock,Gd,TA,Gd,ALQ,978,Unf,0,284,1262,GasA,Ex,Y,SBrkr,1262,0,0,1262,0,1,2,0,3,1,TA,6,Typ,1,TA,Attchd,1976,RFn,2,460,TA,TA,Y,298,0,0,0,0,0,NA,NA,NA,0,5,2007,WD,Normal,181500"))

(define (read-line a-line)
  (for/vector ([token (string-split a-line ",")]) (maybe-string->number token)))

(define (maybe-string->number a-string)
  (let ([maybe-number (string->number a-string)])
    (if maybe-number
        maybe-number
        ; else
        a-string)))

(module+ test
  (test-case "Read a line of input"
    (let ([parsed (read-line sample-line)])
      (check-true (vector? parsed))
      (let ([first-element (vector-ref parsed 0)]
            [secnd-element (vector-ref parsed 1)]
            [third-element (vector-ref parsed 2)])
        (check-eq? first-element 1)
        (check-eq? secnd-element 60)
        (check-equal? third-element "RL")))))

(define (read-file in)
  (for/vector ([a-line (in-lines in)]
               [index (in-naturals)]
               #:when (> index 0))
    (read-line a-line)))

(module+ test
  (test-case "Read a file of input"
    (let ([parsed (read-file (open-input-string sample-file))])
      (check-equal? (vector-ref parsed 0) (read-line sample-line))
      (check-eq? (vector-length parsed) 2))))

;; TODO: not all possible values are seen in the train data
(define (unique-values vvs index)
  (set->list (for/set ([v vvs]) (vector-ref v index))))

(define (one-hot-encode unique-vals element)
  (let ([bitfield (make-vector (length unique-vals) #f)])
    (vector-set! bitfield (index-of unique-vals element) #t)
    bitfield))

(define (one-hot-encode-column! vvs index)
  (let ([uniques (unique-values vvs index)])
    (for ([v vvs])
      (vector-set! v index (one-hot-encode uniques (vector-ref v index))))))

(module+ test
  (test-case "One-hot encode a column"
    (let ([miniparsed (vector (vector 1 60 "RL") (vector 2 20 "RH"))])
      (check-equal? (unique-values miniparsed 2) '("RH" "RL"))
      (let ([uniques (unique-values miniparsed 2)])
        (check-equal? (one-hot-encode uniques "RL") #(#f #t))
        (begin
          (one-hot-encode-column! miniparsed 2)
          (check-equal? miniparsed '#(#(1 60 #(#f #t)) #(2 20 #(#t #f)))))))))


(define (is-categorical-column? vvs index)
  (for/and ([v vvs]) (string? (vector-ref v index))))

(define (find-all-categorical-columns vvs)
  (let ([row-length (vector-length (vector-ref vvs 0))])
    (for/list ([i (in-range row-length)]
               #:when (is-categorical-column? vvs i))
      i)))

(module+ test
  (test-case "Find all columns that are categorical"
    (let ([input #(#(1 60 "RL") #(2 20 "RH"))])
      (check-false (is-categorical-column? input 1))
      (check-true (is-categorical-column? input 2))
      (check-equal? (find-all-categorical-columns input) '(2)))))

(define (one-hot-encode-all-columns! vvs)
  (for ([categorical-column-index (find-all-categorical-columns vvs)])
    (one-hot-encode-column! vvs categorical-column-index)))

(define (parse-file file-name)
  (let ([M (read-file (open-input-file file-name))])
    (one-hot-encode-all-columns! M)
    M))
