(define (sub-terms L1 L2)
  (add-terms L1 (negated-terms L2)))

;; -- 分子の最大次数の項を、分母の最大次数の項で割る => x^5 / x^2 = x^3
;; -- 結果に分母を掛けたものを、分子から引く => (x^5 - 1) - x^3 * (x^2 - 1) = x^3 - 1
;; -- これを新たな分子だと考えて、同じことを繰り返す
;; --- x^3 / x^2 = x
;; --- (x^3 - 1) - x * (x^2 - 1) = x - 1
;; -- 分子の次数が、分母の次数以下になれば、終了
;; --- x^3 + x 余り x - 1
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
