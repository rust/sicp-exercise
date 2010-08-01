;; x^5 + 2x^4 + 3x^2 -2x -5 -> (1 2 0 3 -2 -5)
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))

(define (the-empty-termlist) '())
(define (first-term term-list)
  (let ((order (length term-list)))  ;; 前から高い次数なので、リストの長さが最大次数のはず
    (list order (car term-list))))
(define (rest-terms term-list)
  (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))