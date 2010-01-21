;; - segments->painter を使って下記のを定義せよ

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; -- フレームの外枠を描画する painter
(define designated-frame
  (let ((s1 (make-vect 0.0 1.0))
        (s2 (make-vect 1.0 1.0))
        (s3 (make-vect 1.0 0.0))
        (s4 (make-vect 0.0 0.0)))
    (segments->painter
     (list (make-segment s1 s2)
           (make-segment s2 s3)
           (make-segment s3 s4)
           (make-segment s4 s1)))))

;; -- フレームの対角線を結んで "X" を描画する painter
(define cross-frame
  (let ((s1 (make-vect 0.0 1.0))
        (s2 (make-vect 1.0 1.0))
        (s3 (make-vect 1.0 0.0))
        (s4 (make-vect 0.0 0.0)))
    (segments->painter
     (list (make-segment s4 s2)
           (make-segment s1 s3)))))

;; -- フレームの各辺の中点を結んだダイアモンド型を描画する painter
(define dismond-frame
  (let ((s1 (make-vect 0.5 0.0))
        (s2 (make-vect 1.0 0.5))
        (s3 (make-vect 0.5 1.0))
        (s4 (make-vect 0.0 0.5)))
    (segments->painter
     (list (make-segment s1 s2)
           (make-segment s2 s3)
           (make-segment s3 s4)
           (make-segment s4 s1)))))

;; -- wave painter
;; ちょっと面倒なので他参照