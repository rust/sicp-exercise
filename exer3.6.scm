(use srfi-27)
(define rand
  (let ((in-x 1))
    (lambda (sym)
      (cond ((eq? sym 'generate)
             (begin (set! in-x (rand-update in-x))
                    in-x))
            ((eq? sym 'reset)
             (lambda (new-init) (set! in-x new-init)))
            (else
             (error "No action found!"))))))

;; 仮実装
(define random-init 12345)
(define (rand-update x)
  (let ((in-x 12345))
    (begin (set! in-x (+ x 1))
           in-x)))

(rand 'generate) ;; => 2
(rand 'generate) ;; => 3
(rand 'generate) ;; => 4
((rand 'reset) 345)
(rand 'generate) ;; => 346
(rand 'generate) ;; => 347
(rand 'generate) ;; => 348
((rand 'reset) 345)
(rand 'generate) ;; => 346
(rand 'generate) ;; => 347
(rand 'generate) ;; => 348
