(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

;; 距離を計算
(define (square x) (* x x))
(define (distance start-point end-point)
  (let ((x-dist (square (- (x-point start-point) (x-point end-point))))
        (y-dist (square (- (y-point start-point) (y-point end-point)))))
    (sqrt (+ x-dist y-dist))))

;;;; 幅と高さで表現
;; 正方形
(define (rectangle upper-segment left-segment)
  (cons upper-segment left-segment))
(define (rectangle-upper rec)
  (car rec))
(define (rectangle-left rec)
  (cdr rec))

;; 幅
(define (rectangle-width rec)
  (let ((upper-segment (rectangle-upper rec)))
    (distance (start-segment upper-segment) (end-segment upper-segment))))
;; 高さ
(define (rectangle-height rec)
  (let ((left-segment (rectangle-left rec)))
    (distance (start-segment left-segment) (end-segment left-segment))))

(define r (rectangle
           (make-segment (make-point 0 5) (make-point 5 5))
           (make-segment (make-point 0 5) (make-point 0 1))))
(rectangle-width r)  ;; => 5.0
(rectangle-height r) ;; => 4.0

;; 周囲
(define (rectangle-perimeter rec)
  (* 2 (+ (rectangle-width rec) (rectangle-height rec))))
;; 面積
(define (rectangle-area rec)
  (* (rectangle-width rec) (rectangle-height rec)))

(rectangle-perimeter r) ;; => 18.0
(rectangle-area r)      ;; => 20.0

;;;; 上下の辺で表現
(define (rectangle-para upper-segment lower-segment)
  (cons upper-segment lower-segment))
(define (rectangle-para-upper rec)
  (car rec))
(define (rectangle-para-lower rec)
  (cdr rec))

;; 幅
(define (rectangle-width rec)
  (let ((upper-segment (rectangle-para-upper rec)))
    (distance (start-segment upper-segment) (end-segment upper-segment))))
(define (rectangle-height rec)
  (let ((upper-start-point (start-segment (rectangle-para-upper rec)))
        (lower-start-point (start-segment (rectangle-para-lower rec)))
        (lower-end-point (end-segment (rectangle-para-lower rec))))
    (let ((ss-distance (distance upper-start-point lower-start-point))
          (se-distance (distance upper-start-point lower-end-point)))
      (if (< ss-distance se-distance)
          ss-distance
          se-distance))))

(define rd (rectangle-para
            (make-segment (make-point 0 5) (make-point 5 5))
            (make-segment (make-point 0 3) (make-point 5 3))))
(rectangle-width rd)  ;; => 5.0
(rectangle-height rd) ;; => 2.0

(rectangle-perimeter rd) ;; => 14.0
(rectangle-area rd)      ;; => 10.0
