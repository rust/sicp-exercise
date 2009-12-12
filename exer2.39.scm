(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) (append (list x) y)) () sequence))
(reverse '(1 2 3 4 5 6))

(define (reverse sequence)
  (fold-left (lambda (x y) (append x (list y))) () sequence))
(reverse '(1 2 3 4 5 6))
