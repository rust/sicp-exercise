(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; normal
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))
;; slow
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size))
;; queen-cols を呼ぶ回数の違い
;; 前者 -> board-size 回
;; 後者
;;   (queen-cols 6) は (queen-cols 5) を 6 回
;;   (queen-cols 5) は (queen-cols 4) を 5 回
;;   (queen-cols 4) は (queen-cols 3) を 4 回
;;   (queen-cols 3) は (queen-cols 2) を 3 回
;;   (queen-cols 2) は (queen-cols 1) を 2 回
;;   (queen-cols 1) は (queen-cols 0) を 1 回
;;   (queen-cols 0) は () を返す
;;     -> 6! -> 30 回
