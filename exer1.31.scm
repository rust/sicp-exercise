;; a.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity n) n)

(product identity 1 inc 10) ;; factorial 10

;; therefore

(define (factorial n)
  (define (inc m) (+ m 1))
  (define (identity m) m)
  (product identity 1 inc n))
(factorial 10)

(define (pi n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))
  (define (inc m) (+ m 1))
  (* 4.0 (product term 1 inc n)))
(pi 2000) ;; => 3.142377365093878

;; b.
;; above procedure is recursive process, so I will write procedure by iterative process.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(product-iter identity 1 inc 10) ;; => 3628800
