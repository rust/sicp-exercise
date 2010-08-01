;; パスワードも含める
(define (make-account balance secret)
  (let ((calls 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch password m)
      (if (eq? secret password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          (lambda (x)
            (begin
              (set! calls (+ calls 1))
              (if (>= calls 7)
                  (call-the-cops)
                  (print "Incorrect password"))))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;; => 60
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50) ;; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
;; => *** ERROR: unbound variable: call-the-cops
;; => Stack Trace:
;; _______________________________________
