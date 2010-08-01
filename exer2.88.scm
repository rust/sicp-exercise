;; 負にして足せばよい
;;   term list の方
(define (negate-terms term-list)
  (define (iter negated-terms terms) ;; 先頭から係数に -1 を掛けて再構築する手続き
    (if (empty? terms)               ;; 空なら
        negated-terms                ;;   -> negated-terms を返して終了
        (iter (adjoin-term (make-term (order (first-term first))
                                      (* -1 (coeff (first-term first))))  ;; 係数を -1 倍した項を作って
                           negated-terms)                                 ;; 追加する
              (rest-terms terms))))           ;; 残りと再帰
  (iter '() term-list))

;; パッケージの方
(define (install-polynomial-package)
  (define (negate p)
    (make-poly (variable p) (negate-terms (term-list p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate p2)))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)
