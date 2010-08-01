(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))
(define (remainder-terms L1 L2)
  (cdr (div-terms L1 L2)))
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

(gcd-terms q1 q2)
(gcd-terms q2 (remainder-terms q1 q2))
;;
(remainder-terms q1 q2)
(cdr (div-terms q1 q2))
;;
(div-terms q1 q2)
(let ((t1 (first-term q1))
      (t2 (first-term q2)))
  (if (> (order t2) (order t1))
      (list (the-empty-termlist) q1)
      (let ((new-c (div (coeff t1) (coeff t2)))
            (new-o (- (order t1) (order t2))))
        (let ((rest-of-result
               (div-terms (sub-terms q1 (mul-terms q2 (make-term new-o new-c))) q2)))
          (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
                (cdr rest-of-result))
          ))))
(let ((new-c (div 11 13))
      (new-o (- 4 3)))
  (let ((rest-of-result
         (div-terms (sub-terms q1 (mul-terms q2 (make-term new-o new-c))) q2)))
    (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
          (cdr rest-of-result))
    ))
(let ((rest-of-result
       (div-terms (sub-terms q1 (mul-terms q2 (make-term 1 ('ratinal 11 13)))) q2)))
  (list (adjoin-term (car rest-of-result) (make-term 1 ('ratinal 11 13)))
        (cdr rest-of-result))
  )
;;
(div-terms (sub-terms q1 (mul-terms q2 '((1 ('ratinal 11 13))))) q2)
;;
(mul-terms q2 '((1 ('ratinal 11 13))))
'((4 11) (3 ('ratinal -231 13)) (2 ('ratinal 891 13)) (1 ('ratinal 385 13)))
;;
(sub-terms q1 '((4 11) (3 ('ratinal -231 13)) (2 ('ratinal 891 13)) (1 ('ratinal 385 13))))
'((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7))
;;
(div-terms '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)) q2)
;;
(let ((t1 (first-term '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7))))
      (t2 (first-term q2)))
  (if (> (order t2) (order t1))
      (list (the-empty-termlist) '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)))
      (let ((new-c (div (coeff t1) (coeff t2)))
            (new-o (- (order t1) (order t2))))
        (let ((rest-of-result
               (div-terms (sub-terms '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)) (mul-terms q2 (make-term new-o new-c))) q2)))
          (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
                (cdr rest-of-result))
          ))))
(let ((rest-of-result
       (div-terms (sub-terms '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)) (mul-terms q2 (make-term 0 ('ratinal -55 169)))) q2)))
  (list (adjoin-term (car rest-of-result) (make-term 0 ('ratinal -55 169)))
        (cdr rest-of-result)))
;;
(div-terms (sub-terms '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)) (mul-terms q2 (make-term 0 ('ratinal -55 169)))) q2)
;;
(mul-terms q2 (make-term 0 ('ratinal -55 169)))
'((3 ('ratinal -55 13)) (2 -1155) (1 -4455) (0 -1925))
;;
(sub-terms '((3 ('ratinal -55 13)) (2 ('ratinal -657 13)) (1 ('ratinal -567 13)) (0 7)) '((3 ('ratinal -55 13)) (2 -1155) (1 -4455) (0 -1925)))
'((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))
;;
(div-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)) q2)
'(() '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
;;
(list (adjoin-term (car '(() '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))) (make-term 0 ('ratinal -55 169)))
      (cdr '(() '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
(list '(0 ('ratinal -55 169))
      '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))
('(0 ('ratinal -55 169)) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
;;
(list (adjoin-term (car ('(0 ('ratinal -55 169)) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))) (make-term 1 ('ratinal 11 13)))
      (cdr ('(0 ('ratinal -55 169)) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
(list (adjoin-term '(0 ('ratinal -55 169)) '(1 ('ratinal 11 13)))
      '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
(list '(1 ('ratinal 11 13)) '(0 ('ratinal -55 169))
      '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
;;
(cdr ('(1 ('ratinal 11 13)) '(0 ('ratinal -55 169)) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))))
'((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))
;;
(gcd-terms q2 '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
(gcd-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))
           (remainder-terms q2 '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))))
;;
(remainder-terms q2 '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
(cdr (div-terms q2 '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932))))
;;
(div-terms q2 '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))
(let ((t1 (first-term q2))
      (t2 (first-term '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
  (if (> (order t2) (order t1))
      (list (the-empty-termlist) q2)
      (let ((new-c (div (coeff t1) (coeff t2)))
            (new-o (- (order t1) (order t2))))
        (let ((rest-of-result
               (div-terms (sub-terms q2 (mul-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)) (make-term new-o new-c))) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
          (list (adjoin-term (car rest-of-result) (make-term new-o new-c))
                (cdr rest-of-result))
          ))))
(let ((rest-of-result
       (div-terms (sub-terms q2 (mul-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)) (make-term 1 ('ratinal -15672 169)))) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
  (list (adjoin-term (car rest-of-result) (make-term 1 ('ratinal -15672 169)))
        (cdr rest-of-result)))
(let ((rest-of-result
       (div-terms (sub-terms q2 (mul-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)) (make-term 1 ('ratinal -15672 169)))) '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)))))
  (list (adjoin-term (car rest-of-result) (make-term 1 ('ratinal -15672 169)))
        (cdr rest-of-result)))
;;
(mul-terms '((2 ('ratinal -15672 13)) (1 ('ratinal 57348 13)) (0 1932)) (make-term 1 ('ratinal -15672 169)))
;; とんでもなく大きな値になりそうなので頓挫

