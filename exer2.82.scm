;; 引数が3つ以上ある時
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))  ;; 最初に型タグを全て取得する => これでループする
    (define (do-coerce x type)            ;; 型変換する。見つからなければエラー。
      (let ((coercion (get-coercion x type)))
        (if coercion
            (coercion x)
            (error "No method for these types"
                   (list op type-tags)))))
    (define (coercion-and-apply-generic op args types)
      (if (null? types) ;; もうなければエラー
          (error "No method for these types"
                 (list op type-tags))
          (let ((type (car types)))                                        ;; 今回使う型
            (let ((coerced-args (map do-coerce args)))                     ;; それで一斉変換
              (let ((proc (get op (map type-tag args))))                   ;; 演算を取得して
                (if proc
                    (apply proc (map contents args))                       ;; 存在していればそれを
                    (coercion-and-apply-generic op args (cdr types)))))))) ;; 存在しなければ次へ
    (let ((proc (get op type-tags)))                         ;; まずは演算があるか調べて
      (if proc
          (apply proc (map contents args))                   ;; あればそれを
          (coercion-and-apply-generic op args type-tags))))) ;; なければ変換してゆく
