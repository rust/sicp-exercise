;; for recursive
(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

(last-pair (list 11 22 33 44)) ;; => (44)