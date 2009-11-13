(define (deep-reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(define x (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)
