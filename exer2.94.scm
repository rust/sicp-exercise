;; div-terms の返値の cdr が余りなので
(define (remainder-terms L1 L2)
  (cdr (div-terms L1 L2)))

;; gcd-terms
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (install-polynomial-package)
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (tag (make-poly
              (variable p1)
              (gcd-terms (term-list p1) (term-list p2))))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))

  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (gcd-poly p1 p2)))
  'done)

;; シミュレーション
(define p1 (make-polyinomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polyinomial 'x '((3 1) (1 -1))))

(greatest-common-divisor p1 p2)
(gcd-poly p1 p2)
(tag (make-poly
      (variable p1)
      (gcd-terms (term-list p1) (term-list p2))))
(tag (make-poly
      'x
      (gcd-terms '((4 1) (3 -1) (2 -2) (1 2)) '((3 1) (1 -1)))))
;; ここで
(gcd-terms '((4 1) (3 -1) (2 -2) (1 2)) '((3 1) (1 -1)))
(gcd-terms '((3 1) (1 -1))
           (remainder-terms
            '((4 1) (3 -1) (2 -2) (1 2))
            '((3 1) (1 -1))))
;; さらに
(remainder-terms
 '((4 1) (3 -1) (2 -2) (1 2))
 '((3 1) (1 -1)))
(cdr (div-terms
      '((4 1) (3 -1) (2 -2) (1 2))
      '((3 1) (1 -1))))
;; 省略して
'((2 -1) (1 1))
;; よって
(gcd-terms '((3 1) (1 -1))
           '((2 -1) (1 1)))
(gcd-terms '((2 -1) (1 1))
           (remainder-terms
            '((3 1) (1 -1))
            '((2 -1) (1 1))))
;; また
(remainder-terms
 '((3 1) (1 -1))
 '((2 -1) (1 1)))
'()
;; よって
(gcd-terms '((2 -1) (1 1)) '())
;; 答え
'((2 -1) (1 1)) ;; => -x^2 + x = -x(x-1)
