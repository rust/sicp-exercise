(use srfi-27)
(define random random-integer)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define PI 3.1415927)
(define ans (/ (* PI 100 100) 4)) ;; => 7853.98175

(define (P x y)
  (>= 10000 (+ (* x x) (* y y))))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (define (iter trial-remaining trial-passed)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (cond ((= trial-remaining 0)
             (/ trial-passed trials))
            ((predicate x y)
             (iter (- trial-remaining 1) (+ trial-passed 1)))
            (else
             (iter (- trial-remaining 1) trial-passed)))))
  (iter trials 0))

(* (estimate-integral P 0 0 100 100 10000000) 10000.0) ;; => 7956.795