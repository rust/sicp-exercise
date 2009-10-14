;; for smooth
(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ ( + (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (identity x) x)
(define (square x) (* x x))

;; n-fold
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-body f i)
    (if (eq? i 1)
        f
        (compose f (repeated-body f (- i 1)))))
  (repeated-body f n))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth square 10) 1)