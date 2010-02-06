(define (lookup given-key set-of-records)
  (let ((ekey (entry set-of-records)))
    (cond ((null? set-of-records) #f)
          ((= given-key ekey) #t)
          ((> given-key ekey) (lookup given-key (left-tree set-of-records)))
          ((< given-key ekey) (lookup given-key (right-tree set-of-records))))))