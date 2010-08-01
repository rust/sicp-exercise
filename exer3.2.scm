;; 引数によって変える
(define (make-monitored func)
  (let ((calls 0))
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls?)
             calls)
            ((eq? arg 'reset-count)
             (begin (set! calls 0)
                    calls))
            (else
             (begin
               (set! calls (+ calls 1))
               (func arg)))))
    mf))

(define s (make-monitored sqrt))
(s 100) ;; => 10
(s 'how-many-calls?) ;; => 1
(s 100) ;; => 10
(s 'how-many-calls?) ;; => 2
(s 'reset-count)
(s 100) ;; => 10
(s 'how-many-calls?) ;; => 2
