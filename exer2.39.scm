(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(reverse '(1 2 3 4 5 6))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))
(reverse '(1 2 3 4 5 6))
