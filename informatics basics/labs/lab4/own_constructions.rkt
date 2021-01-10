(define-syntax when
  (syntax-rules ()
    ((when cond? expr1 ...)
     (if cond? (begin expr1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? expr1 ...)
     (if (not cond?) (begin expr1 ...)))))

; TEST (when) (unless)
(display "TEST (when) (unless)") (newline)
(define x 1)
(when   (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))

(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr1 ...)
     (let loop ((vals xs))
       (if (not (null? vals))
           (let ((i (car vals)))
             expr1 ...
             (loop (cdr vals))))))
    ((for xs as i expr1 ...)
     (for i in xs expr1 ...))))

; TEST (for)
(display "TEST (for)") (newline)
(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))


(define-syntax while
  (syntax-rules ()
    ((while cond? expr1 ...)
     (let loop ()
       (if cond? (begin expr1 ... (loop)))))))

; TEST (while)
(display "TEST (while)") (newline)
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))



(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr1 ...) until cond?)
     (let loop ()
       expr1 ...
       (if (not cond?) (loop))))))

; TEST (repeat .. unitl)
(display "TEST (repeat .. unitl)") (newline)
(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))


(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . expressions)
     (begin (newline)
            (cout . expressions)))
    ((cout << expr1)
     (display expr1))
    ((cout << expr1 . expressions)
     (begin (display expr1)
            (cout . expressions)))))

; TEST (cout)
(display "TEST (cout)") (newline)
(cout << "a = " << 10)
(cout << endl << "a = " << 1 << endl << "b = " << 2 << endl)