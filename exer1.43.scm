(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-body f i)
    (if (eq? i 1)
        f
        (compose f (repeated-body f (- i 1)))))
  (repeated-body f n))

((repeated square 2) 5)