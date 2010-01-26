(define (adjoin-set x set)
  (let ((sx1 (car set)))
    (cond ((null? set) (list x))
          ((< x sx1) (cons x set))
          ((= x sx1) set)
          (else
           (cons sx1 (adjoin-set x (cdr set)))))))

(adjoin-set 3 '(1 2 4 5 6))

