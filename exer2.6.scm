(define (zero (lambda (f) (lambda (x) x))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; one
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) y) x))))
(lambda (f) (lambda (x) (f x)))
;; therefore
(define (one (lambda (f) (lambda (x) (f x)))))

;; two
(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
(lambda (f) (lambda (x) (f (f x))))
;; therefore
(define (two (lambda (f) (lambda (x) (f (f x))))))

;; plus
(lambda (f) (lambda (x) (f    x ))) ;; => one
(lambda (f) (lambda (x) (f (f x)))) ;; => two

;;
(one f)
((lambda (f) (lambda (x) (f x))) f)
(lambda (x) (f x))
;;
((one f) x)
((lambda (x) (f x)) x)
(f x)
;;
((one f) ((one f) x))
((lambda (x) (f x)) (f x))
(f (f x))
;; therefore
(define (plus n m) (lambda (f) (lambda (x) ((n f) ((m f) x)))))
(plus one one)
(lambda (f) (lambda (x) ((one f) ((one f) x))))
(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g y))) f) (((lambda (h) (lambda (z) (h z))) f) x))))
(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g y))) f) ((lambda (z) (f z)) x))))
(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g y))) f) (f x))))
(lambda (f) (lambda (x) ((lambda (y) (f y)) (f x))))
(lambda (f) (lambda (x) (f (f x)))) ;; => two
