(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ;; => 1024
(A 2 4)  ;; => 65536
(A 3 3)  ;; => 65536
(A 0 10) ;; => 20
(A 0 5)  ;; => 10

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; (f n) => (A 0 n)
(A 0 n)
(cond ((= n 0) 0)
      ((= 0 0) (* 2 n))
      ((= n 1) 2)
      (else (A (- 0 1)
               (A 0 (- n 1)))))
(* 2 n) ;; (f n) => 2n

;; (g n) => (A 1 n)
(A 1 n)
(cond ((= n 0) 0)
        ((= 1 0) (* 2 n))
        ((= n 1) 2)
        (else (A (- 1 1)
                 (A 1 (- n 1)))))
(A (- 1 1) (A 1 (- n 1)))
(A 0 (A 1 (- n 1)))
(A 0 (A 0 (A 1 (- n 2))))
(A 0 (A 0 (A 0 (A 1 (- n 3)))))
(A 0 (A 0 (A 0 ... (A 0 (A 1 (- n (n-1)))) ... )))
(A 0 (A 0 (A 0 ... (A 0 (A 1 1)) ... )))
(A 0 (A 0 (A 0 ... (A 0 2) ... )))
(A 0 (A 0 (A 0 ... (* 2 2) ... )))
;; (A 0 x) を n - 1 - 1 回
;;   -> 2 を n 回掛ける
;;   -> 2^n
(g 32)
4294967296 ;; 2^32

;; (h n) => (A 2 n)
