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
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"  ;; なければエラー
                                (list op type-tags))))))
              (error "No method for these types" ;; 引数が2個より多いので give up
                     (list op type-tags)))))))

;; a. この状況でシミュレーションする
(define x (cons 'complex (cons 'rectangular (cons 1 2))))
(define y (cons 'complex (cons 'rectangular (cons 3 4))))
(exp x y)
(apply-generic 'exp x y)
(let ((type-tags (map type-tag (list x y))))
  (let ((proc (get 'exp type-tags)))
    (if proc
        (apply proc (map contents (list x y)))
        (if (= (length (list x y)) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car (list x y)))
                  (a2 (cadr (list x y))))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                       (apply-generic 'exp (t1->t2 a1) a2))
                      (t2->t1
                       (apply-generic 'exp a1 (t2->t1 a2)))
                      (else
                       (error "No method for these types"
                              (list 'exp type-tags))))))
            (error "No method for these types"
                   (list 'exp type-tags))))))
;; proc がないので
(if (= (length (list x y)) 2)
    (let ((type1 (car type-tags))
          (type2 (cadr type-tags))
          (a1 (car (list x y)))
          (a2 (cadr (list x y))))
      (let ((t1->t2 (get-coercion type1 type2))
            (t2->t1 (get-coercion type2 type1)))
        (cond (t1->t2
               (apply-generic 'exp (t1->t2 a1) a2))
              (t2->t1
               (apply-generic 'exp a1 (t2->t1 a2)))
              (else
               (error "No method for these types"
                      (list 'exp type-tags))))))
    (error "No method for these types"
           (list 'exp type-tags)))
;; 引数は2つだから
(let ((type1 (car type-tags))
      (type2 (cadr type-tags))
      (a1 (car (list x y)))
      (a2 (cadr (list x y))))
  (let ((t1->t2 (get-coercion type1 type2))
        (t2->t1 (get-coercion type2 type1)))
    (cond (t1->t2
           (apply-generic 'exp (t1->t2 a1) a2))
          (t2->t1
           (apply-generic 'exp a1 (t2->t1 a2)))
          (else
           (error "No method for these types"
                  (list 'exp type-tags))))))
;; type1, type2, a1, a2 を適用して
(let ((t1->t2 (get-coercion 'complex 'complex))
      (t2->t1 (get-coercion 'complex 'complex)))
  (cond (t1->t2
         (apply-generic 'exp (t1->t2 x) y))
        (t2->t1
         (apply-generic 'exp x (t2->t1 y)))
        (else
         (error "No method for these types"
                (list 'exp type-tags))))))
;; t1->t2 があるので
(apply-generic 'exp (t1->t2 x) y)
;; t1->t2 は結局何もしないので
(apply-generic 'exp x y)
;; 以下無限ループ

;; 要するに同じ型なら変換しないようにすればいい
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
                (if (= type1 type2)                     ;; 型が同じであれば、そこで終わり
                    (error "No method for these types"
                           (list op type-tags))
                    ((let ((t1->t2 (get-coercion type1 type2))    ;; 1つ目から2つ目への変換
                           (t2->t1 (get-coercion type2 type1)))   ;; 2つ目から1つ目への変換
                       (cond  (t1->t2                              ;; 存在する方を実行
                               (apply-generic op (t1->t2 a1) a2))
                              (t2->t1
                               (apply-generic op a1 (t2->t1 a2)))
                              (else
                               (error "No method for these types"  ;; なければエラー
                                      (list op type-tags)))))))
                (error "No method for these types" ;; 引数が2個より多いので give up
                       (list op type-tags)))))))
