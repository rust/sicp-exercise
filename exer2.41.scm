(define (enumerate-interval low high)
    (if (> low high) '() (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list j i))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k)
           (list k j i))
         (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (triplet-eq? triplet s)
  (eq? s (+ (car triplet) (cadr triplet) (caddr triplet))))
(define (make-triplet-sum triplet)
  (append triplet (+ (+ (car triplet) (cadr triplet) (caddr triplet)))))

(define (sum-triplet n s)
  (map make-triplet-sum (filter (lambda (t) (triplet-eq? t s))
                                (unique-triples n))))
