10
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(if (and (> b a) (< b (* a b)))
    b
    a)
(+ 2 (if (> b a) b a))
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(define (sq x y) (+ (* x x) (* y y)))
(define (<= x y) (not (> x y)))
(define (sq3 x y z)
  (cond ((and (<= x y) (<= x z)) (sq y z))
        ((and (<= y x) (<= y z)) (sq x z))
        (else (sq x y))))
(sq3 3 4 5)
(sq3 3 3 3)
(sq3 9 8 7)

(define (square x) (* x x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)

(define (new-if predicate the-clause else-clause)
  (cond (predicate the-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(sqrt 9)
