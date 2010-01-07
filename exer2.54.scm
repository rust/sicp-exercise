(equal? '(this is a list) '(this is a list)) ;; => #t

(equal? '(this is a list) '(this (is a) list)) ;; => #f
