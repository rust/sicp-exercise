;; recursive
(define (cont-frac n d k)
  (define (kterm n d i)
    (if (< k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (kterm n d (+ i 1))))))
  (kterm n d 1))

(define (tan-cf x k)
  (define (tan-n i)
    (if (eq? i 1)
        x
        (- (* x x))))
  (define (tan-d i)
    (- (* i 2) 1))
  (cont-frac tan-n tan-d k))

(define (pi) 3.14159265358979323846)
(tan (/ (pi) 4)) ;; => 0.9999999999999999 -> 1.0
(tan-cf (/ (pi) 4) 100) ;; => 1.0
(tan-cf (+ (/ (pi) 2) (/ (pi) 4)) 100) ;; => -1.0
(tan-cf (+ (pi) (/ (pi) 4)) 100) ;; => 0.9999999999999999
