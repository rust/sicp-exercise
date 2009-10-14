(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment segment)
  (cons (/ (+ (x-point (start-segment segment))
              (x-point (end-segment segment)))
           2)
        (/ (+ (y-point (start-segment segment))
              (y-point (end-segment segment)))
           2)))

(define (s)
  (make-point 1 1))
(print-point (s))                      ;; => (1,1)
(define (e)
  (make-point 2 2))
(print-point (e))                      ;; => (2,2)
(define (seg)
  (make-segment (s) (e)))
(print-point (midpoint-segment (seg))) ;; => (3/2,3/2)