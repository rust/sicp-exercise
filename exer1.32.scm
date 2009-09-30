;; recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (inc n) (+ n 1))
(define (identity n) n)

(sum identity 1 inc 10)

;; iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (inc n) (+ n 1))
(define (identity n) n)

(sum-iter identity 1 inc 10)
