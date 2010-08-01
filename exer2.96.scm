(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))
(define (pseudoremainder-terms L1 L2)
  (let ((o1 (order (first-term L1)))
        (o2 (order (first-term L2)))
        (c (coeff (first-term L1))))
    (let ((factor (expt (- (+ 1 O1) O2))))
      (cdr (div-terms (mul factor L1) L2)))))
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))      ;; 分母の次数が分子よりも大きければ
            (list (the-empty-termlist) L1) ;; 商は空リスト、余りは分子を返す
            (let ((new-c (div (coeff t1) (coeff t2)))  ;; 分母と分子の最大次数の、係数の商
                  (new-o (- (order t1) (order t2))))   ;; 分母と分子の最大次数の、次数の商
              (let ((rest-of-result
                     (div-terms (sub-terms L1 (mul-terms L2 (make-term new-o new-c))) L2)))
                (list (adjoin-term (car rest-of-result) (make-term new-o new-c))  ;; 商は加算して返す
                      (cdr rest-of-result))                                       ;; 余りはそのまま返す
                ))))))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define q1 (make-poly 'x '((4 11) (3 -22) (2 18) (1 -14) (0 7))))
(define q2 (make-poly 'x '((3 13) (2 -21) (1 81) (0 35))))
;; GCD
(gcd-terms q1 q2)
(gcd-terms q2 (pseudoremainder-terms q1 q2))
;;
(pseudoremainder-terms q1 q2)
(cdr (div-terms (mul 169 q1) q2))
;;
(div-terms (mul 169 q1) q2)
(div-terms (make-poly 'x '((4 (* 169 11)) (3 (* 169 -22)) (2 (* 169 18)) (1 (* 169 -14)) (0 (* 169 7))))
           q2)
;; (make-poly 'x '((4 (* 169 11)) (3 (* 169 -22)) (2 (* 169 18)) (1 (* 169 -14)) (0 (* 169 7)))) => q1d
(let ((new-c (div (* 169 11) 13))
      (new-o (- 4 3)))
  (let ((rest-of-result
         (div-terms (sub-terms q1d (mul-terms q2 (make-term new-o new-c))) q2)))
    (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
          (cdr rest-of-result))
    ))
(let ((new-c (* 13 11))
      (new-o 1))
  (let ((rest-of-result
         (div-terms (sub-terms q1d (mul-terms q2 (make-term new-o new-c))) q2)))
    (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
          (cdr rest-of-result))
    ))
(let ((rest-of-result
       (div-terms (sub-terms q1d (mul-terms q2 (make-term 1 (* 13 11)))) q2)))
  (list (adjoin-term (car rest-of-result) (make-term 1 (* 13 11)))
        (cdr rest-of-result))
  )
;;
(mul-terms q2 (make-term 1 (* 13 11)))
(make-poly 'x '((4 (* 169 11)) (3 (* 13 11 -21)) (2 (* 13 11 81)) (1 (* 13 11 35))))
;;
(sub-terms q1d (mul-terms q2 (make-term 1 (* 13 11))))
(sub-terms
 (make-poly 'x '((4 (* 169 11)) (3 (* 169 -22)) (2 (* 169 18)) (1 (* 169 -14)) (0 (* 169 7))))
 (make-poly 'x '((4 (* 169 11)) (3 (* 13 11 -21)) (2 (* 13 11 81)) (1 (* 13 11 35)))))
(make-poly 'x '((3 (* 13 -55)) (2 (* 13 -657)) (1 (* 13 -567)) (0 (* 13 13 7))))
;; この時点で有理数になっていないので、整数因子が有効に働いていることがわかる。

;; 要するに pseudoremainder-terms の戻り値を factor で割れば元に戻るはず
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (let ((o1 (order (first-term a)))
            (o2 (order (first-term b)))
            (c (coeff (first-term a))))
        (let ((factor (expt (- (+ 1 O1) O2))))
          (gcd-terms b (div (pseudoremainder-terms a b) factor))))))
