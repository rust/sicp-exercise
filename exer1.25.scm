(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (expmod base exp n)
  (remainder (fast-expt base exp) n))

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
(search-for-primes 10000)
(search-for-primes 100000) ;; 終わらない
(search-for-primes 1000000)

;; 検証開始
;; before
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
;; new
(define (expmod base exp n)
  (remainder (fast-expt base exp) n))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;; n = 100001, a = 2 のときを検証
;;   base -> 2
;;   exp  -> 100001
;;   n    -> 100001
;;
;; for new-version
(expmod 2 100001 100001)
(remainder (fast-expt 2 100001) 100001)
(remainder (cond ((= 100001 0) 1)
                 ((even? 100001) (square (fast-expt 2 (/ 100001 2))))
                 (else (* 2 (fast-expt 2 (- 100001 1))))) 100001)
(remainder (* 2 (fast-expt 2 (- 100001 1))) 100001)
