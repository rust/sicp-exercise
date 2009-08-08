(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(sine 12.15)
(if (not (> (abs 12.15) 0.1))
    12.15
    (p (sine (/ 12.15 3.0))))
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (if (not (> (abs 4.05) 0.1))
       4.05
       (p (sine (/ 4.05 3.0)))))
(p (p (sine (/ 4.05 3.0))))
(p (p (sine 1.35)))
(p (p (if (not (> (abs 1.35) 0.1))
          1.35
          (p (sine (/ 1.35 3.0))))))
(p (p (p (sine (/ 1.35 3.0)))))
(p (p (p (sine 0.45))))
(p (p (p (if (not (> (abs 0.45) 0.1))
             0.45
             (p (sine (/ 0.45 3.0)))))))
(p (p (p (p (sine (/ 0.45 3.0))))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (if (not (> (abs 0.15) 0.1))
                0.15
                (p (sine (/ 0.15 3.0))))))))
(p (p (p (p (p (sine (/ 0.15 3.0)))))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
