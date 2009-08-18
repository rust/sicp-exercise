(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 10 9) ;; #=> 90

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
(fast-mul 2 3)  ;; => 6
(fast-mul 2 12) ;; => 24
(fast-mul 2 13) ;; => 26
