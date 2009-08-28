(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))

(define (simp-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k)
    (f (* (h) (+ a k))))
  (define (cy k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum cy 1 inc n) (/ (h) 3.0)))

(define (identity x) x)
(simp-integral identity 0 1 100)  ;; => 0.5
(simp-integral identity 0 1 1000) ;; => 0.5

(define (cube x) (* x x x))
(simp-integral cube 0 1 100)  ;; => 0.25
(simp-integral cube 0 1 1000) ;; => 0.25
