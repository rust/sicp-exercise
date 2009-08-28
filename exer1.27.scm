;; 561

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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (random x)
  (modulo (sys-random) x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 561 10)

(define (all-fermat-test n)
  (fermat-test-iter n (- n 1)))
(define (fermat-test-iter n counter)
  (cond ((= counter 1) #f)
        ((= (expmod counter n n) counter) #t)
        (else (fermat-test-iter n (- counter 1)))))

(all-fermat-test 561) ;; => #t
(smallest-divisor 561) ;; => 3