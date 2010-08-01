;; 共通項を計算して、それで割った結果を返せばよいはず。
(define (reduce-term n d)
  (let ((common-term (gcd-terms n d)))
    (let ((nn (car (div-terms n common-term)))
          (dd (car (div-terms d common-term))))
      (list nn dd))))

;; add-poly の add-terms を reduce-terms に変更すればよいだけ。
(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (reduce-terms (term-list p1)
                               (term-list p2)))
      (error "Polys not in same var -- REDUCE-POLY"
             (list p1 p2))))

;; 汎用演算を定義
(put 'reduce ('polynomial 'polynomial) reduce-poly)
(put 'reduce ('scheme-number 'scheme-number) reduce-intergers)

;; reduce を呼んで、その結果を make-rat に渡せば良いだけ
(define (make-rational a b)
  (let ((results (reduce a b)))
    (make-rat (car results) (cdr results))))
