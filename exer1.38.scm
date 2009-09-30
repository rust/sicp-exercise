;; recursive
(define (cont-frac n d k)
  (define (kterm n d i)
    (if (< k i)
        0.0
        (/ (n i) (+ (d i) (kterm n d (+ i 1))))))
  (kterm n d 1))

;;   1, 2, 1, 1, 4, 1, 1, 6, 1,  1, 8, 1, ....
;;   1  2  3  4  5  6  7  8  9  10 11 12
;; r 1  2  0  1  2  0  1  2  0   1  2  0
;; q 0  0  1  1  1  2  2  2  3   3  3  4
(define (eular-d i)
  (let ((r (remainder i 3))
        (q (quotient i 3)))
    (if (eq? r 2)
        (+ r (* q 2))
        1)))

(+ (cont-frac (lambda (x) 1.0) eular-d 100) 2)
