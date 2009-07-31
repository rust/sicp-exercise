;; recursive process
(define (f n)
  (cond ((<= n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;; iterative process
