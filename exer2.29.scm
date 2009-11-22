(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; b.
(define (total-weight mobile)
  (define (total-weight-branch branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(define m (make-mobile
           (make-branch 3
                        (make-mobile
                         (make-branch 2 4)
                         (make-branch 3 4)))
           (make-branch 5 8)))

(total-weight m) ;; => 4 + 4 + 8 = 16

;; c.
(define not-balanced-mobile (make-mobile
                             (make-branch 3 9)
                             (make-branch 6 8)))
(define balanced-mobile (make-mobile
                         (make-branch 3 9)
                         (make-branch 9 3)))
(define (balanced-mobile? mobile)
  (define (total-weight-branch branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (= (* (total-weight-branch (left-branch mobile)) (branch-length (left-branch mobile)))
     (* (total-weight-branch (right-branch mobile)) (branch-length (right-branch mobile)))))

(balanced-mobile? not-balanced-mobile) ;; => #f
(balanced-mobile? balanced-mobile)     ;; => #t

;; d.
;; only modify selectors.
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))


