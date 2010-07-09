(define (make-account balance secret)
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
        (error "Incorrect password"
               password)))
  dispatch)

(define (make-joint acc secret joint-secret)
  (define (dispatch password m)
    (if (eq? joint-secret password)
        (acc secret m)
        (error "Incorrect joint-password"
               password)))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 30) ;; =>  70
((paul-acc 'rosebud 'deposit) 100) ;; => 170
