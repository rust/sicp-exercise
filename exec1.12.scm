;; Pascal's triangle
;; n
;; 1        1
;; 2       1 1
;; 3      1 2 1
;; 4     1 3 3 1
;; 5    1 4 6 4 1
(define (coeff n m)
  (if (< n m)
      0
      (cond ((= m 1) 1)
            ((= m n) 1)
            (else (+ (coeff (- n 1) (- m 1)) (coeff (- n 1) m))))))

(coeff 1 1) ;; 1
(coeff 2 1) ;; 1
(coeff 3 1) ;; 1
(coeff 4 1) ;; 1
(coeff 2 2) ;; 1
(coeff 3 2) ;; 2
(coeff 4 3) ;; 3
(coeff 5 3) ;; 6
(coeff 6 4) ;; 10
(coeff 7 3) ;; 15
(coeff 7 4) ;; 20

