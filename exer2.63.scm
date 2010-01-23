;; 順に集めて返すパターン
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
;; 要素をひとつずつ見るパターン
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; a. 正しい二分木なら同じ結果を返すはず
(tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(tree->list-2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; b.
;;   tree->list-2 は n 個の要素を集めるから -> θ(n)
;;   tree->list-1 は append で θ(n) かかり，
;;                   ツリーを下まで走査するのに θ(log n) かかるから
;;     -> θ(n log n)