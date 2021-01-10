(define (trib n)
  (let ((memo (make-vector (+ n 1))))
    (let loop ((n n))
      (cond
        ((<= n 1) 0)
        ((= n 2) 1)
        (else (if (zero? (vector-ref memo n))
                  (vector-set! memo n
                               (+ (loop (- n 1))
                                  (loop (- n 2))
                                  (loop (- n 3)))))
              (vector-ref memo n))))))

(define (trib-no-memo n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    (else (+ (trib-no-memo (- n 1)) (trib-no-memo (- n 2)) (trib-no-memo (- n 3))))))
