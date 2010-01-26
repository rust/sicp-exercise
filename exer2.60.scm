;; element-of-set?, intersection-set は同じ
;; adjoin-set, union-set は普通に結合するだけ
(define (adjoin-set x set1) (cons x set1))
(define (union-set set1 set1) (append set1 set2))