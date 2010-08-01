;; integer / rational / real / complex パッケージはここでは書かない
;; for interger
(define (install-integer-package)
  (define (integer->rational n)
    (make-rational n 1))
  (put 'raise '(integer)
       (lambda (n) (integer->rational n)))
  'done)
;; for rational
(define (install-rational-package)
  (define (rational->real x)
    (make-real (/ (numer x) (denom) x)))
  (put 'raise '(rational)
       (lambda (x) (rational->real x)))
  'done)
;; for real
(define (install-real-package)
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  (put 'raise '(real)
       (lambda (x) (real->complex x)))
  'done)

(define (raise x) (apply-generic 'raise x))
