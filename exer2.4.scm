(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(car (cons "a" "b"))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons "a" "b"))
