(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right / 1 (list 1 2 3)) ;; => 3/2

(fold-left / 1 (list 1 2 3)) ;; => 1/6

(fold-right list () (list 1 2 3)) ;; => (1 (2 (3 ())))

(fold-left list () (list 1 2 3)) ;; => (((() 1) 2) 3)

;; fold-left と fold-right で同じ結果になる op は，たとえば + と * で，
;; 演算子の左右の値を入れ替えても結果が変わらないもの