(define (solve a b c)
  
  (define (d a b c)
    (- (* b b) (* 4 a c)))
  
  (if (< (d a b c) 0)
      (list)
      (if (= (d a b c) 0)
          (list (/ (- b) (* 2 a)))
          (list (/ (+ (- b) (sqrt (d a b c))) (* 2 a))
                (/ (- (- b) (sqrt (d a b c))) (* 2 a))))))
                  