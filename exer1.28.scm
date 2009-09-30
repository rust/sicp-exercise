(define (square x)
  (* x x ))
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
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (runtime)
  (- (time->seconds (current-time)) 1136041200))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      #f))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)
(define (search-for-primes n)
  (if (timed-prime-test n)
      #t
      (search-for-primes (+ n 1))))

(search-for-primes 1000)
;; 1000
;; 1001
;; 1002
;; 1003
;; 1004
;; 1005
;; 1006
;; 1007
;; 1008
;; 1009 *** 0.001722097396850586#t

(search-for-primes 10007)
;; 10000
;; 10001
;; 10002
;; 10003
;; 10004
;; 10005
;; 10006
;; 10007 *** 0.0022661685943603516#t
