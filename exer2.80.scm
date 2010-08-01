;; 2.79と同様に各パッケージに追加する
;;   普通の数
(define (install-scheme-number-package)
  (define (=zero? x) (= x 0))

  (put '=zero? '(scheme-number scheme-number) =zero?))
;;   有理数
(define (install-rational-package)
  (define (=zero? x) (= (numer x) 0))

  (put '=zero? '(rational rational) =zero?))
;;   複素数
(define (install-complex-package)
  (define (=zero? z)
    (or (= (magnitude z) 0)
        (and (= (real-part z) 0) (= (imag-part z) 0))))

  (put '=zero? '(complex complex) =zero?))

;; apply-generic で定義する
(define (=zero? x) (apply-generic '=zero? x))
