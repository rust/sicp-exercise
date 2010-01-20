(define (my-equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b)) (my-equal? (cdr a) (cdr b))))
        (else (eq? a b))))

(my-equal? '(this is a list) '(this is a list)) ;; => #t
(my-equal? '(this is a list) '(this (is a) list)) ;; => #f
