(define (make-interval a b) (cons a b))
(define (upper-bound a)
  (car a))
(define (lower-bound a)
  (cdr a))

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

(define int1 (make-interval 3.0 4.0))
(define int0 (make-interval 2.0 2.0))

(div-interval int1 int0)