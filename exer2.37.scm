(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define v (list 1 2 3))
(define w (list 4 5 6))
(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define n (list (list 1 3 9) (list 2 4 8) (list 5 2 0)))

;; 内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product v w) ;; => 32

;; 行列とベクトルの積
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(matrix-*-vector m v) ;; => (14 32 50)

;; 転地
(define (transpose mat)
  (accumulate-n cons () mat))
(transpose m) ;; => ((1 4 7) (2 5 8) (3 6 9))

;; 行列同士の積
            (map proc (cdr items)))))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
(matrix-*-matrix m n) ;; => ((20 17 25) (44 44 76) (68 71 127))
