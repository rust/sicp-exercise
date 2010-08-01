;; 他はどこかで実装済という前提

;; drop
(define (drop x)
  (cond ((eq? (type-tag x) 'integer) x)  ;; 整数はそのまま
        (else (let ((dropped-x (project x)))    ;; 下位に落とす
                (if (equ? x (raise dropped-x))  ;; 落として raise したものと値が同じかどうか
                    (drop dropped)              ;; 同じならまだ落とせるかも
                    x)))))                      ;; 異なれば、この drop は失敗。前のを返す。

;; rational から integer への project
(define (install-rational-package)
  (define (rational->integer x)
    (make-integer (round (/ (numer x) (denom x)))))
  (put 'project '(rational)
       (lambda (x) (rational->integer x)))
  'done)

;; real から rational への project
(define (install-real-package)
  (define (real->rational x)
    (make-rational ????))               ;; 複雑すぎて頓挫
  (put 'project '(real)
       (lambda (x) (real->rational x)))
  'done)

;; complex から real への project
(define (install-complex-package)
  (define (complex->real z)
    (make-real (real-part z)))
  (put 'project '(complex)
       (lambda (x) (complex->real z)))
  'done)

;; 汎用演算
(define (project x) (apply-generic 'project x))

;; 次は apply-generic を
;;   まず構造のリスト
(define (hierarchy-of-type '(complex real rational integer)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (raise-type x type)
      (if (eq? (type-tag x) type)       ;; raise しきった？
          x                             ;; なら答えを
          (raise-type (raise x) type))) ;; さらに raise する
    (define (highest-type args-type-list)          ;; 引数の型リストの中で最大のものを取得
      (define (exist-type types target-type)  ;; target-type が types の中にあるかどうかチェック
        (if (null? types)
            #f
            (if (eq? (car types) target-type)
                #t
                (exist-type (cdr types) target-type))))
      (define (iter hierarchy-type-list)
        (if (null? hierarchy-type-list)                                ;; 変換先がないと
            (error "Can't raise type"                                  ;; エラーに
                   (list op args))
            (if (exist-type args-type-list (car hierarchy-type-list))  ;; その型が存在するかどうか
                (car hierarchy-type-list)                              ;; 存在すればそれを
                (iter (cdr hierarchy-type-list)))))                    ;; なければ次のを
      (iter hierarchy-of-type))                      ;; 定義された型ヒエラルキーリストで始める
    (define (droppable x)                         ;; 簡略化できればする
      (if (memq op '(equ? =zero? raise project))  ;; op がリストの中にあれば
          (lambda (x) x)                          ;; 実行して返すだけ
          drop))                                  ;; そうでなければ drop する
    (let ((proc (get op type-tags)))
      (if proc
          (droppable (apply proc (map contents args)))      ;; 最初の型でOKならそれを
          (let ((highest (highest-type type-tags)))                        ;; 最も高レベルの型
            (let ((raised-args (map (lambda (x) (raise-type x highest))))) ;; その型で統一する
              (let ((raised-type-tags (map type-tag raised-args)))         ;; その型のタグで
                (let ((raised-proc (get op raised-type-tags)))             ;; 演算を取得し
                  (if raised-proc
                      (droppable (apply raised-proc (map contents raised-args))) ;; 実行
                      (error "No method for these types"                         ;; なければエラー
                             (list op type-tags)))))))))))
