(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(expt 2 31) ;; => 2147483648

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
(expt 2 31) ;; => 4294967296

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))
(fast-expt 2 31) ;; => 2147483648

;; Exercise 1.16
(define (expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b)
                                         (/ counter 2)
                                         product))
        (else (fast-expt-iter b
                              (- counter 1)
                              (* b product)))))
(expt 2 31) ;; => 2147483648

;; Exercise 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 10 12) ;; => 120

(define (* a b)

