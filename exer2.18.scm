(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(reverse (list 1 2 3 4 5 6)) ;; => (6 5 4 3 2 1)
