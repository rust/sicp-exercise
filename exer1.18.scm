;; 1.16
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
;; 1.17
(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

;; therefore
(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (fast-mul-iter a b 0))
(define (fast-mul-iter a counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-mul-iter (double a)
                                        (halve counter)
                                        product))
        (else (fast-mul-iter a
                             (- counter 1)
                             (+ a product)))))
(* 1 2) ;; => 2
(* 20 13) ;; => 260
(* 14 213) ;; => 2982