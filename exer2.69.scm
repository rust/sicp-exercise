(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-ordered-pairs
  (make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1))))
(define (successive-merge ordered-pairs)
  (define (reversed-successive-merge reversed-pairs)
    (if (null? (cdr reversed-pairs))
        (car reversed-pairs)
        (make-code-tree (car reversed-pairs)
                        (reversed-successive-merge (cdr reversed-pairs)))))
  (reversed-successive-merge (reverse ordered-pairs)))
(print (successive-merge sample-ordered-pairs))
;; => ((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8)
(print sample-tree)
;; => ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
