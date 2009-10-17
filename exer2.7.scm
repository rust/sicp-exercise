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

(define int-a (make-interval 12 10))
(upper-interval int-a) ;; => 13.1
(lower-interval int-a) ;; => 10.8
