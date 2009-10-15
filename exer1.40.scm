;; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (print count " " guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))

;; derivative
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

;; newton-method
(define (newton-transoform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transoform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(newton-method (cubic 1 1 1) 1) ;; => -0.9999999999997796
