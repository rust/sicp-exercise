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
(define int-a (make-interval 10 12))
(define int-b (make-interval 8 18))

;; 幅
(define (interval-width i)
  (/ (+ (upper-bound i) (lower-bound i)) 2.0))

;; 加算
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(interval-width (add-interval int-a int-b)) ;; => 24.0

(define (add-interval-width x y)
  (+ (interval-width x) (interval-width y)))
(add-interval-width int-a int-b)            ;; => 24.0

;; 減算
(define (sub-interval x y)
  (let ((p1 (- (upper-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y))))
    (make-interval (min p1 p2)
                   (max p1 p2))))

(interval-width (sub-interval int-a int-b)) ;; => -2.0

(define (sub-interval-width x y)
  (- (interval-width x) (interval-width y)))
(sub-interval-width int-a int-b)            ;; => -2.0

;; 乗算
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(interval-width (mul-interval int-a int-b)) ;; => 148.0

(define (mul-interval-width x y)
  (* (interval-width x) (interval-width y)))
(mul-interval-width int-a int-b)            ;; => 143.0

;; 除算
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(interval-width (div-interval int-a int-b)) ;; => 1.0277777777777777

(define (div-interval-width x y)
  (/ (interval-width x) (interval-width y)))
(div-interval-width int-a int-b)            ;; => 0.8461538461538461
