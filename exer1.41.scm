(define (inc x) (+ x 1))

(define (double procedure)
  (lambda (x)
    (print 1)
    (procedure (procedure x))))

((double inc) 5) ;; => 5

(define (square x) (* x x))
((double square) 5)

(((double (double double)) inc) 5) ;; => 21

(((double double) inc) 5) ;; => 9





