(define (square x)
  (* x x ))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n ) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divide? a b)
  (= (remainder b a) 0))

(smallest-divisor 127)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(expmod 13 4 4)

(use srfi-27) ;; for procedure of `random-integer`
(random-integer 100)
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
;; true -> #t, false -> #f
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 401 1000)
