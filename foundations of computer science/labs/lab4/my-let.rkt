(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name1 value1) ...) expr1 ...)
     ((lambda (name1 ...) expr1 ...) value1 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((name1 value1)) expr1 ...)
     (my-let ((name1 value1)) expr1 ...))
    ((my-let* ((name1 value1) (name2 value2) ...) expr1 ...)
     (my-let ((name1 value1))
             (my-let* ((name2 value2) ...) expr1 ...)))))

(my-let* () 10)
(my-let* ((a 900)) a)
(my-let* ((a 10) (b (+ a a))) (+ a b))
