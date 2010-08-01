;; 多項式がゼロ => 多項式の係数がすべてゼロ or term-list が empty?
(define (install-polynomial-package)
  (define (=zero? p)
    (if (or (empty? (term-list p))  ;; 空なら
            (= (accumulate + 0 (map coeff term-list)) 0))  ;; 係数の和が0なら
        #t
        #f))
  'done)
