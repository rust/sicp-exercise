(define (make-interval a b) (cons a b))
(define (lower-bound int) (car int))
(define (upper-bound int) (cdr int))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define int1 (make-interval 2.0 4.0))
(define int2 (make-interval 3.0 5.0))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(par1 int1 int2) ;; => (0.6666666666666666 . 4.0)
(par2 int1 int2) ;; => (1.2000000000000002 . 2.2222222222222223)
;; 確かに異なる

;;                     int1 => (2.0 4.0)
(div-interval int1 int1) ;; => (0.5 2.0)
(define int3 (make-interval 4.0 8.0))
(div-interval int3 int1) ;; => (1.0 4.0)
;; 確かに異なる
;; A/A => (1.0 1.0) になるべき

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100)))
    (make-interval (- c w) (+ c w))))
(define (percent int)
  (let ((c (/ (+ (upper-bound int) (lower-bound int)) 2.0)))
    (* (/ (abs (- (upper-bound int) c)) c) 100.0)))
(define (center int)
  (/ (+ (upper-bound int) (lower-bound int)) 2.0))

(define cint1 (make-center-percent 10 1))
(define cint2 (make-center-percent 15 1))

(print cint1)              ;; => (99/10 . 101/10)
(div-interval cint1 cint1) ;; => (0.9801980198019803 . 1.02020202020202)
;; ちょっとずれる
