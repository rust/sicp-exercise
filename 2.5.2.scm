;; coercion
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;; 型タグを取り出す
    (let ((proc (get op type-tags)))     ;; 演算があるか
      (if proc
          (apply proc (map contents args))  ;; あれば適用
          (if (= (length args) 2)           ;; なければ、引数が2個であれば続ける
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))    ;; 1つ目から2つ目への変換
                      (t2->t1 (get-coercion type2 type1)))   ;; 2つ目から1つ目への変換
                  (cond (t1->t2                              ;; 存在する方を実行
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a2 (t2->t1 a2)))
                        (else
                         (error "No method for these types"  ;; なければエラー
                                (list op type-tags))))))
              (error "No method for these types" ;; 引数が2個より多いので give up
                     (list op type-tags)))))))

