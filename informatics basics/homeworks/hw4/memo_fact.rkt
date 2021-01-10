(define fact
  (let loop ((memo '()))
    (lambda (n)
      (if (<= n 1) n
          (let ((memo-val (assoc n memo)))
            (if memo-val
                (cadr memo-val)
                (let ((res (* n (fact (- n 1)))))
                  (set! memo (cons (list n res) memo))
                  res)))))))
  