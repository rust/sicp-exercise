(define (square x) (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items ()))
(square-list (list 1 2 3 4 5)) ;; => (25 16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))
(square-list (list 1 2 3 4 5)) ;; => (((((() . 1) . 4) . 9) . 16) . 25)
;; 実際に追ってみる

(square-list (list 1 2 3 4 5))
(iter  ())
(if (null? (1 2 3 4 5))
    ()
    (iter (cdr (1 2 3 4 5))
          (cons ()
                (square (car (1 2 3 4 5))))))
(iter (cdr (1 2 3 4 5))
      (cons ()
            (square (car (1 2 3 4 5)))))
(iter (2 3 4 5)
      (cons ()
            (square 1)))
(iter (2 3 4 5)
      (cons ()
            (square 1)))
(iter (2 3 4 5) (cons () 1))
(iter (cdr (2 3 4 5))
      (cons (cons () 1)
            (square (car (2 3 4 5)))))
(iter (3 4 5) (cons (cons () 1) 4))
;; 結局
(cons (cons (cons (cons (cons () 1) 4) 9) 16) 25)
;; となってしまい，本来の
;; (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 ())))))
;; とは違う形になるから