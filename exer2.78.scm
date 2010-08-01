;; 要するに、数だったら 'scheme-number と言うことにすれば良いはず
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else "Unknown datum type -- TYPE-TAG" datum)))
;; タグを付けないようにすればいい
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
;; 数値だったらそのまま返すようにする
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else "Unknown datum type -- CONTENTS" datum)))
