(define (same-parity head . remain)
  (define (extract-same-parity predicate lst)
    (cond ((null? lst) ())
          ((predicate (car lst))
           (append (list (car lst)) (extract-same-parity predicate (cdr lst))))
          (else (extract-same-parity predicate (cdr lst)))))
  (cond ((even? head) (append (list head) (extract-same-parity even? remain)))
        ((odd?  head) (append (list head) (extract-same-parity odd?  remain)))
        (else (error "Error"))))

(same-parity 1 2 3 4 5 6 7) ;; => (1 3 5 7)