(define (sqrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? last-guess guess)
  (< (abs (- last-guess guess)) 0.001))

(sqrt-iter 0.0 1.0 2)
(sqrt-iter 0.0 1.0 123456)

(define (cbrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
      guess
      (cbrt-iter guess (improve guess x)
                 x)))
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? last-guess guess)
  (< (abs (- last-guess guess)) 0.001))

(cbrt-iter 0.0 1.0 2)

(cbrt-iter 0.0 1.0 1000)


(/ (+ (/ 10 (* 10 10)) (* 2 10)) 3)