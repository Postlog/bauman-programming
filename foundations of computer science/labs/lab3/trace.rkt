(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex obj) (begin (display (quote obj))
                           (display " => ")
                           (let ((x obj)) 
                             (begin (display x)
                                    (newline)
                                    x))))))

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss)))))) ; ... и здесь

    

