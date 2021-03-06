(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (xcor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w)) (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w)) (- (ycor-vect v) (ycor-vect w))))
(define (scale-vect v a)
  (make-vect (* a (xcor-vect v)) (* a (ycor-vect v))))

(define (make-segment start-vector end-vector)
  (cons start-vector end-vector))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
