;; まず構造のリスト
(define (hierarchy-of-type '(complex real rational integer)))

;; 次に apply-generic
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
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))      ;; 最初の型でOKならそれを
          (let ((highest (highest-type type-tags)))                        ;; 最も高レベルの型
            (let ((raised-args (map (lambda (x) (raise-type x highest))))) ;; その型で統一する
              (let ((raised-type-tags (map type-tag raised-args)))         ;; その型のタグで
                (let ((raised-proc (get op raised-type-tags)))             ;; 演算を取得し
                  (if raised-proc
                      (apply raised-proc (map contents raised-args))       ;; 実行
                      (error "No method for these types"                   ;; なければエラー
                             (list op type-tags)))))))))))
