(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(define (square x) (* x x ))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (square-tree tree) (tree-map square tree))

(square-tree t) ;; => (1 (4 (9 16) 25) (36 49))

