(define (make-interval a b) (cons a b))
(define (upper-bound a)
  (let ((p1 (car a))
        (p2 (cdr a)))
    (if (< p1 p2)
        p1
        p2)))
(define (lower-bound a)
  (let ((p1 (car a))
        (p2 (cdr a)))
    (if (< p1 p2 )
        p2
        p1)))

(define (sub-interval x y)
  (let ((p1 (- (upper-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y))))
    (make-interval (min p1 p2)
                   (max p1 p2))))

(define int-a (make-interval 10 12))
(define int-b (make-interval 8 18))

(sub-interval int-a int-b) ;; => (-8 . 4)
