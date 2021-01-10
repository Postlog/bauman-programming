(define top 0)

(define-syntax use-assertations
  (syntax-rules ()
    ((use-assertations) (call-with-current-continuation
                         (lambda (stack) (set! top stack))))))

(define-syntax assert
  (syntax-rules ()
    ((assert expr) (if (not expr)
                       (begin (display "FAILED: ")
                              (display (quote expr)) (newline)
                              (top))))))

(use-assertations)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))