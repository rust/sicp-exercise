;; 下準備
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bat bit -- CHOOSE BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left-tree (left-branch tree))
            (right-tree (right-branch tree)))
        (cond ((element-of-set? symbol (symbols left-tree))
               (cons '0 (encode-symbol symbol left-tree)))
              ((element-of-set? symbol (symbols right-tree))
               (cons '1 (encode-symbol symbol right-tree)))
              (else (error "not found -- ENCODE SYMBOL" symbol))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge ordered-pairs)
  (define (reversed-successive-merge reversed-pairs)
    (if (null? (cdr reversed-pairs))
        (car reversed-pairs)
        (make-code-tree (car reversed-pairs)
                        (reversed-successive-merge (cdr reversed-pairs)))))
  (reversed-successive-merge (reverse ordered-pairs)))

;; (define rock-pairs
;;   (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2) '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1)))
(define rock-pairs
  (list '(a 2) '(boom 1) '(Get 2) '(job 2) '(na 16) '(Sha 3) '(yip 9) '(Wah 1)))
(define rock-tree (generate-huffman-tree rock-pairs))
((leaf na 16)
 ((leaf yip 9)
  ((leaf Sha 3)
   ((leaf a 2)
    ((leaf Get 2)
     ((leaf job 2)
      ((leaf boom 1)
       (leaf Wah 1)
       (boom Wah) 2)
      (job boom Wah) 4)
     (Get job boom Wah) 6)
    (a Get job boom Wah) 8)
   (Sha a Get job boom Wah) 11)
  (yip Sha a Get job boom Wah) 20)
 (na yip Sha a Get job boom Wah) 36)

(define rock-message
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip
    Sha boom))
(define rock-encoded-bits (encode rock-message rock-tree))
;; => (1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 0)
(length rock-encoded-bits) ;; => 85

;; by 8-symbols fixed-length
;;   8-symbols        -> each symbol needs 3 bits
;;   words in message -> 3 + 9 + 3 + 9 + 9 + 2 -> 35
;;   bits             -> 35 x 3 -> 105 bits
