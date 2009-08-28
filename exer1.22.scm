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
(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime)
  (- (time->seconds (current-time)) 1136041200))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(timed-prime-test 13)

(define (search-for-primes n)
  (if (timed-prime-test n)
      #t
      (search-for-primes (+ n 1))))


(search-for-primes 100)     ;; 1.5974044799804687e-5
(search-for-primes 1000)    ;; 3.504753112792969e-5
(search-for-primes 10000)   ;; 9.799003601074219e-5
(search-for-primes 100000)  ;; 2.8705596923828125e-4
(search-for-primes 1000000) ;; 8.749961853027344e-4
;; だいたい sqrt3 = 1.73 倍ずつ増えてはいる

(search-for-primes 10000000000)      ;;  0.08550214767456055
(search-for-primes 100000000000)     ;;  0.37103986740112305
(search-for-primes 1000000000000)    ;;  1.060863971710205
(search-for-primes 10000000000000)   ;;  3.364123821258545
(search-for-primes 100000000000000)  ;; 10.508246898651123
(search-for-primes 1000000000000000) ;; 32.68144512176514
;; 確かに 2 桁で 10 倍になっている

(search-for-primes 100) ;; => 101
(search-for-primes 102) ;; => 103
(search-for-primes 104) ;; => 107
(search-for-primes 1001) ;; => 1009
(search-for-primes 1010) ;; => 1013
(search-for-primes 1014) ;; => 1019
(search-for-primes 10001) ;; => 10007
(search-for-primes 10008) ;; => 10009
(search-for-primes 10010) ;; => 10037
(search-for-primes 100001) ;; => 100003
(search-for-primes 100004) ;; => 100019
(search-for-primes 100020) ;; => 100043
(search-for-primes 1000001) ;; => 1000003
(search-for-primes 1000004) ;; => 1000033
(search-for-primes 1000034) ;; => 1000037
