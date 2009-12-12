;; before
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 4)))) ;; => 4

;; after
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1)) t)))
(count-leaves (list 1 (list 2 (list 3 4)))) ;; => 4
