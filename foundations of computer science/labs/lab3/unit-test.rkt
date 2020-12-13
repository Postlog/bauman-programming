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


(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  0) ; Ошибка здесь!
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  1)
        (test (signum  2)  1)))
(run-tests the-tests)