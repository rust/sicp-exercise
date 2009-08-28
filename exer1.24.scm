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

;; => 101 (1.6927719116210937e-5)
;; => 103 (1.6927719116210937e-5)
;; => 107 (2.002716064453125e-5)
;; => 1009 (3.218650817871094e-5)
;; => 1013 (3.504753112792969e-5)
;; => 1019 (3.409385681152344e-5)
;; => 10007 (9.584426879882812e-5)
;; => 10009 (9.512901306152344e-5)
;; => 10037 (9.489059448242187e-5)
;; => 100003 (2.8705596923828125e-4)
;; => 100019 (2.86102294921875e-4)
;; => 100043 (2.8705596923828125e-4)
;; => 1000003 (8.828639984130859e-4)
;; => 1000033 (9.000301361083984e-4)
;; => 1000037 (8.85009765625e-4)
(search-for-primes 100) ;; => 4.889965057373047e-4
(search-for-primes 102) ;; => 5.021095275878906e-4
(search-for-primes 104) ;; => 0.006179094314575195
(search-for-primes 1001) ;; => 0.0018041133880615234
(search-for-primes 1010) ;; => 0.0018398761749267578
(search-for-primes 1014) ;; => 0.001859903335571289
(search-for-primes 10001) ;; => 0.0023610591888427734
(search-for-primes 10008) ;; => 0.002407073974609375
(search-for-primes 10010) ;; => 0.002409219741821289
(search-for-primes 100001) ;; => 0.005645036697387695
(search-for-primes 100004) ;; => 0.005937814712524414
(search-for-primes 100020) ;; => 0.00595402717590332
(search-for-primes 1000001) ;; => 0.007854938507080078
(search-for-primes 1000004) ;; => 0.012240886688232422
(search-for-primes 1000034) ;; => 0.007517099380493164

(/ 0.007854938507080078 0.0018041133880615234) ;; => 4.353905114312145
(/ (log 1000000) (log 1000)) ;; => 2.0