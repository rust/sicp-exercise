;; recursive
(define (cont-frac n d k)
  (define (kterm n d i)
    (if (< k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (kterm n d (+ i 1))))))
  (kterm n d 1))

(define tolerance 0.0001)
(define golden-ratio 1.6180327868852458)
(define (check-golden-ratio k)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try j)
    (let ((next (/ 1.0 (cont-frac (lambda (i) 1.0)
                                  (lambda (i) 1.0) j))))
      (print j " " next)
      (if (close-enough? golden-ratio next)
          next
          (try (+ j 1)))))
  (try 1))

(check-golden-ratio 20)

;; iterative
(define (cont-frac n d k)
  (define (kterm i term)
    (cond ((<= i 1) term)
          (else (kterm (- i 1) (/ (n i) (+ (d i) term))))))
  (kterm k 0.0))

(/ 1.0 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0) 20))

(define tolerance 0.0001)
(define golden-ratio 1.6180327868852458)
(define (check-golden-ratio k)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try j)
    (let ((next (/ 1.0 (cont-frac (lambda (i) 1.0)
                                  (lambda (i) 1.0) j))))
      (print j " " next)
      (if (close-enough? golden-ratio next)
          next
          (try (+ j 1)))))
  (try 1))

(check-golden-ratio 20)
