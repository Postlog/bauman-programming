(define (factorize expr)
  (let ((a (cadr (cadr expr))) (b (cadr (caddr expr))))
    (cond
      ((and (equal? (caddr (cadr expr)) 2) (equal? (car expr) '-))
       (list '* (list '- a b) (list '+ a b)))
      ((and (equal? (caddr (cadr expr)) 3) (equal? (car expr) '-))
       (list '* (list '- a b) (list '+ `(expt ,a 2) (list '* a b) `(expt ,b 2))))
      ((and (equal? (caddr (cadr expr)) 3) (equal? (car expr) '+))
       (list '* (list '+ a b) (list '+ `(expt ,a 2) (list '- (list '* a b)) `(expt ,b 2))))
      (else expr))))

(factorize '(- (expt x 2) (expt y 2)))
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
(eval (list (list 'lambda 
                      '(x y) 
                      (factorize '(- (expt x 2) (expt y 2))))
                1 2)
          (interaction-environment))
