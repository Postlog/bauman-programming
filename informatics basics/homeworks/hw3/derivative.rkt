(define (derivative expr)
  (define (action-derivative-worker action expr)
    (cond
      ((and (equal? action '*) (number? (car expr))) ; C*func(x) -> c * f(func(x))
       (if (> (length expr) 2)
           (list '* (car expr) (derivative-worker (cons '* (cdr expr))))
           (list '* (car expr) (derivative-worker (cadr expr)))))
      ((equal? action '*) ; func1(x) * func2(x) -> f(func1(x)) * func2(x) + func1(x) * f(func2(x))
       (list '+ (list '* (derivative-worker (car expr)) (cadr expr))
             (list '* (car expr) (derivative-worker (cadr expr)))))
      ((equal? action '/) ; func1(x) / func2(x) -> f(func1(x)) * func2(x) - func1(x) * f(func2(x)) / func2(x)^2
       (list '/ (list '- (list '* (derivative-worker (car expr)) (cadr expr))
                      (list '* (car expr) (derivative-worker (cadr expr))))
             (list 'expt (cadr expr) 2)))
      ((and (equal? action 'expt) (equal? (car expr) 'x)) ; x^n -> n * x^(n-1)
       (list '* (cadr expr) (list 'expt 'x (- (cadr expr) 1))))
      ((and (equal? action 'expt) (number? (car expr))) ; n^x -> n^x * ln(n)
       (list '* expr (list 'log (car expr))))
      ((equal? action 'sin) ; sin(func(x)) -> cos(func(x)) * f(func(x))
       (list '* (list 'cos (car expr)) (derivative-worker (car expr))))
      ((equal? action 'cos) ; cos(func(x)) -> -sin(func(x)) * f(func(x))
       (list '* (list '-sin (car expr)) (derivative-worker (car expr))))
      ((equal? action 'exp) ; exp(func(x)) -> exp(func(x)) * f(func(x))
       (list '* (list 'exp (car expr)) (derivative-worker (car expr))))
      ((equal? action 'log) ; log(func(x)) -> f(func(x)) / func(x)
       (list '/ (derivative-worker (car expr)) (car expr)))))
  
  (define (derivative-worker expr)
    (if (not (list? expr)) (set! expr (list expr)))
    (cond
      ((number? (car expr)) 0)
      ((equal? (car expr) 'x) 1)
      ((equal? (car expr) '-x) -1)
      (else (action-derivative-worker (car expr) (cdr expr)))))

  (define (derivative-loop exprs)
    (if (null? exprs)
        '()
        (cons (derivative-worker (car exprs)) (derivative-loop (cdr exprs)))))
  
  (if (or (equal? (car expr) '+) (equal? (car expr) '-))
      (let ((sign (car expr)))
        (cons sign (derivative-loop (cdr expr))))
      (derivative-worker expr)))

;;; TESTS
(define-syntax test
  (syntax-rules ()
    ((test call res) (list (quote call) res))))

(define (run-test test)
  (display (car test))
  (define res (eval (car test) (interaction-environment)))
  (if (equal? res (cadr test))
      (begin (display " ok") (newline) #t)
      (begin (display " FAIL") (newline)
             (display "\tExpected: ") (display (cadr test)) (newline)
             (display "\tReturned: ") (display res) (newline)
             #f)))

(define (fand x y) (and x y))
(define (run-tests tests)
  (or (null? tests)
      (fand (run-test (car tests))
            (run-tests (cdr tests)))))

(define derivative-tests
  (list (test (derivative '(2)) '0) ;1
        (test (derivative '(x)) '1) ;2
        (test (derivative '(-x)) '-1) ;3
        (test (derivative '(* 1 x)) '(* 1 1)) ;4
        (test (derivative '(* -1 x)) '(* -1 1)) ;5
        (test (derivative '(* -4 x)) '(* -4 1)) ;6
        (test (derivative '(* 10 x)) '(* 10 1))
        (test (derivative '(- (* 2 x) 3)) '(- (* 2 1) 0)) ;8
        (test (derivative '(expt x 10)) '(* 10 (expt x 9))) ;9
        (test (derivative '(* 2 (expt x 5))) '(* 2 (* 5 (expt x 4)))) ;10
        (test (derivative '(expt x -2)) '(* -2 (expt x -3))) ;11
        (test (derivative '(expt 5 x)) '(* (5 x) (log 5))) ;12
        (test (derivative '(cos x)) '(* (-sin x) 1)) ;13
        (test (derivative '(sin x)) '(* (cos x) 1)) ;14
        (test (derivative '(exp x)) '(* (exp x) 1)) ;15
        (test (derivative '(* 2 (exp x))) '(* 2 (* (exp x) 1))) ;16
        (test (derivative '(* 2 (exp (* 2 x)))) '(* 2 (* (exp (* 2 x)) (* 2 1)))) ;17
        (test (derivative '(log x)) '(/ 1 x)) ;18
        (test (derivative '(* 3 (log x))) '(* 3 (/ 1 x))) ;19
        (test (derivative '(+ (expt x 3) (expt x 2)))
              '(+ (* 3 (expt x 2)) (* 2 (expt x 1)))) ;20
        (test (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2))))
              '(- (* 2 (* 3 (expt x 2))) (* 2 (* 2 (expt x 1))))) ;21
        (test (derivative '(/ 3 x)) '(/ (- (* 0 x) (* 3 1)) (expt x 2))) ;22
        (test (derivative '(* 3/2 (expt x -2))) '(* 3/2 (* -2 (expt x -3)))) ;23
        (test (derivative '(* 2 (sin x) (cos x)))
              '(* 2 (+ (* (* (cos x) 1) (cos x)) (* (sin x) (* (-sin x) 1))))) ;24
        (test (derivative '(* 2 (exp x) (sin x) (cos x)))
              '(* 2 (+ (* (* (exp x) 1) (sin x)) (* (exp x) (* (cos x) 1))))) ;25
        (test (derivative '(sin (* 2 x))) '(* (cos (* 2 x)) (* 2 1))) ;26
        (test (derivative '(cos (* 2 (expt x 2))))
              '(* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1))))) ;27
        (test (derivative '(sin (log (expt x 2))))
              '(* (cos (log (expt x 2))) (/ (* 2 (expt x 1)) (expt x 2)))) ;28
        (test (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              '(+ (* (cos (* 2 x)) (* 2 1)) (* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1)))))) ;29
        (test (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2)))))
              '(+ (* (* (cos (* 2 x)) (* 2 1)) (cos (* 2 (expt x 2)))) (* (sin (* 2 x)) (* (-sin (* 2 (expt x 2))) (* 2 (* 2 (expt x 1))))))) ; 30
        ))

(run-tests derivative-tests)