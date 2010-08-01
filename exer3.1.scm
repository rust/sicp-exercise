;; 要するに make-account の様に作ればよいはず
(define (make-accumulator n)
  (define (sum m)
    (begin (set! n (+ m n))
           n))
  sum)
