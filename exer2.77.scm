;; z => ('complex '(rectangular '(3 4)))

(magnitude z)
(apply-generic 'magnitude z)
;; => (get '(complex) 'magnitude) が見つからないのでエラーだった
(apply magnitude (map contents (list z)))
(apply-generic 'magnitude '(rectangular '(3 4)))
(apply magnitude (map contents (list '(rectangular '(3 4)))))
(sqrt (+ (square (real-part '(3 4)))
         (square (imag-part '(3 4)))))
5
