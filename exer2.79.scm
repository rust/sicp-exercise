;; 各パッケージに追加する
;;   普通の数
(define (install-scheme-number-package)
  (define (equ? x y) (= x y))

  (put 'equ? '(scheme-number scheme-number) equ?))
;;   有理数
(define (install-rational-package)
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

  (put 'equ? '(rational rational) equ?))
;;   複素数
(define (install-complex-package)
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))

  (put 'equ? '(complex complex) equ?))

;; apply-generic で定義する
(define (equ? x y) (apply-generic 'equ? x y))