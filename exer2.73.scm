(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; 例えば x + 3 だと
(deriv '(+ x 3) 'x)
(cond ((number? '(+ x 3)) 0)
        ((variable? '(+ x 3)) (if (same-variable? '(+ x 3) 'x) 1 0))
        (else
         ((get 'deriv (operator '(+ x 3))) (operands '(+ x 3)) 'x)))
((get 'deriv (operator '(+ x 3))) (operands '(+ x 3)) 'x)
((get 'deriv '+) '(x 3) 'x)

;; なのでパッケージは
(define (install-derive-package)
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (sum-of-deriv args var)
    (make-sum (deriv (car args) var)
              (deriv (cadr args) var)))
  (define (product-of-deriv args var)
    (make-sum
     (make-product (car args)
                   (deriv (cadr args) var))
     (make-product (deriv (car args) var)
                   (cadr args))))

  (put '+ 'deriv sum-of-deriv)
  (put '* 'deriv product-of-deriv)
  'done)

;; すると先程の続きは
((get 'deriv '+) '(x 3) 'x)
(sum-of-deriv '(x 3) 'x)
(make-sum (deriv (car '(x 3)) 'x)
          (deriv (cadr '(x 3)) 'x))
(make-sum (deriv 'x 'x)
          (deriv 3 'x))
(make-sum 1 0)
'(+ 1 0)

;; 指数関数は
(define (make-exponentiation b e)
   (cond ((and (number? b) (number? a)) (expt b a))
         ((=number? e 1) b)
         ((=number? e 0) 1)
         (else (list '** b e))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
;; deriv には
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))))

;; こんな感じなので
(define (install-deriv-exponantiation-package)
  (define (make-exponentiation b e)
    (cond ((and (number? b) (number? a)) (expt b a))
          ((=number? e 1) b)
          ((=number? e 0) 1)
          (else (list '** b e))))
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (define (make-product m1 m2) (list '* m1 m2))

  (define (exponentiation-of-deriv args var)
    (make-product
     (car args)
     (make-product
      (make-exponentiation (car args) (- (cadr args) 1))
      (deriv (car args) var))))

  (put 'deriv '** exponentiation-of-deriv)
  'done)

