(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a. どう動くかを調べよ
;;   実際にやってみる
(list->tree '(1 3 5 7 9 11))
(car (partial-tree '(1 3 5 7 9 11) 6))
(partial-tree '(1 3 5 7 9 11) 6)
(cons (make-tree (car (cdr (partial-tree '(1 3 5 7 9 11) 2)))
                 (car (partial-tree '(1 3 5 7 9 11) 2))
                 (car (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 2))) 3)))
      (cdr (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 2))) 3)))
;;
(partial-tree '(1 3 5 7 9 11) 2)
(cons (make-tree (car (cdr (partial-tree'(1 3 5 7 9 11) 0)))
                 (car (partial-tree '(1 3 5 7 9 11) 0))
                 (car (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 1)))
      (cdr (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 1))))
;;
(partial-tree'(1 3 5 7 9 11) 0)
'(() 1 3 5 7 9 11)
;;
(partial-tree'(1 3 5 7 9 11) 1)
'((1 () ()) 3 5 7 9 11)
;;
(partial-tree '(1 3 5 7 9 11) 2)
(cons (make-tree 1 '() '(5 () ())) '(5 7 9 11))
;;
(partial-tree '(1 3 5 7 9 11) 6)
(cons (make-tree '(5 7 9 11)
                 (make-tree 1 '() '(5 () ()))
                 (car (partial-tree '(7 9 11) 3)))
      (cdr (partial-tree '(7 9 11) 3)))
;; 以下繰り返す

;; b. n 要素の場合のステップ数のオーダーを調べよ
;; ノード毎に1回 make-tree が呼ばれるの θ(n)
