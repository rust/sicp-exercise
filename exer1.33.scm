(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (true n) #t)
(define (sum term a next b)
  (filtered-accumulate true + 0 term a next b))

(define (inc n) (+ n 1))
(define (identity n) n)

(sum identity 1 inc 10) ;; => 55

;; sum of squared integer from a to b.
(define (prime? n)
  (define (square x)
    (* x x ))
  (define (divide? a b)
    (= (remainder b a) 0))
  (define (find-divisor k test-divisor)
    (cond ((> (square test-divisor) k) k)
          ((divide? test-divisor k ) test-divisor)
          (else (find-divisor k (+ test-divisor 1)))))
  (define (smallest-divisor k)
    (find-divisor k 2))
  (= n (smallest-divisor n)))

(prime? 101)

(define (square-prime-sum a b)
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (filtered-accumulate prime? + 0 square a inc b))

(square-prime-sum 1 10) ;; => 88

(define (square x) (* x x ))
(+ (square 1) (square 2) (square 3) (square 5) (square 7)) ;; => 88

;; products of all integers that a relatively prime to n
(define (relatively-prime-product n)
  (define (identity k) k)
  (define (inc k) (+ k 1))
  (define (gcd a n)
    (if (= n 0)
        a
        (gcd n (remainder a n))))
  (define (relatively-prime k n)
    (if (= (gcd k n) 1)
        #t
        #f))
  (filtered-accumulate relatively-prime * 1 identity 1 inc n))

(relatively-prime-product 5)  ;; => 24

(relatively-prime-prudoct 10) ;; => 189
(* 1 3 7 9)                   ;; => 189
