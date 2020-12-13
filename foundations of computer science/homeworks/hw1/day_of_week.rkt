(define (calculate d m c y)
  (modulo (+ d (floor (- (* 2.6 m) 0.2)) (- (* 2 c)) y (floor (/ y 4)) (floor (/ c 4))) 7))

(define (day-of-week d m y)
  (define (inner_day-of-week d m y)
    (if (or (= m 1) (= m 2))
        (calculate d (+ m 10) (quotient y 100) (- (modulo y 100) 1))
        (calculate d (modulo (- m 2) 12) (quotient y 100) (modulo y 100))))

  (if (zero? (inner_day-of-week d m y))
      7
      (inexact->exact (inner_day-of-week d m y))))


