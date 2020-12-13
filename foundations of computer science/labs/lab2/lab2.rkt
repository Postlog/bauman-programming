(define (count x xs)
  (if (null? xs)
      0
      (if (equal? (car xs) x)
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))

(define (delete pred? xs)
  (if (null? xs)
      (list)
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))

(define (iterate f x n)
  (if (= n 0)
      (list)
      (cons x (iterate f (f x) (- n 1)))))

(define (intersperse e xs)
  (if (<= (length xs) 1)
      xs
      (append (list (car xs) e) (intersperse e (cdr xs)))))

(define (any? pred? xs)
  (and (not (null? xs)) (or (pred? (car xs)) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (or (null? xs) (and (pred? (car xs)) (all? pred? (cdr xs)))))

(define (o . args)
  (if (null? args)
      (lambda (x) x)
      (lambda (x) ((car args) ((apply o (cdr args)) x)))))