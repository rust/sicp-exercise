;; recursive process
(define (f n)
  (cond ((<= n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;; iterative process
(define (f-n n)
  (cond ((<= n 3) n)
        (else (f-iter 3 2 1 1 (- n 3)))))

(define (f-iter f3 f2 f1 counter max-count)
  (if (> counter max-count)
      f3
      (f-iter (+ f3 (* 2 f2) (* 3 f1)) f3 f2 (+ counter 1) max-count)))