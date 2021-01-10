(define force-return 0)
(define (exit reason)
  ; (display reason) (newline)
  (force-return #f))

(define (find-tail xs target)
  (if (equal? (car xs) target)
      (cdr xs)
      (find-tail (cdr xs) target)))

(define (find-head xs target)
  (let loop ((xs xs) (acc '()))
    (if (null? xs)
        #f
        (if (equal? (car xs) target)
            acc
            (loop (cdr xs) (push acc (car xs)))))))

(define (last xs)
  (list-ref xs (- (length xs) 1)))

(define (tail-endif program)
  (let loop ((program program) (depth -1))
    
    (if (null? program) #f
        (let ((word (car program)))
          (cond
            ((and (equal? word 'endif) (zero? depth)) (cdr program))
            ((equal? word 'endif) (loop (cdr program) (- depth 1)))
            ((equal? word 'if) (loop (cdr program) (+ depth 1)))
            (else (loop (cdr program) depth)))))))

(define (push xs x)
  (append xs (list x)))

(define (head xs n)
  (if (or (= n -1) (null? xs))
      '()
      (cons (car xs) (head (cdr xs) (- n 1)))))

(define (parse program)
  (call-with-current-continuation
   (lambda (stack)
     (set! force-return stack)
     (let ((program (vector->list program)))
       (if (equal? (car program) 'define)
           (let ((articles (parse-articles program)))
             (cons (head articles (- (length articles) 2))
                   (list (parse-body (last articles)))))
           (cons '() (list (parse-body program))))))))


(define (parse-articles program)
  (let loop ((program program))
    (if (not (null? program))
        (let ((word (car program)) (other (cdr program)))
          (if (equal? word 'define)
              (if (null? other) (exit "articles1")
                  (if (member (car other) '(if endif)) (exit "articles2")
                      (let ((head (find-head (cdr other) 'end)))
                        (if (not head) (exit "article31")
                            (cons (cons (car other) (list (parse-body head)))
                                  (loop (find-tail (cdr other) 'end)))))))
              (list program)))
        (list program))))
            
(define (parse-body program)
  (let loop ((program program) (parsed '()) (stack '()))
    (if (not (null? program))
        (let ((word (car program)))
          (cond
            ((equal? word 'if)
             (let ((tail (tail-endif program)))
               (if (not tail) (exit "body1")
                   (loop tail (push parsed (list 'if (loop (cdr program) '() (cons 'if stack)))) stack))))
            ((equal? word 'endif)
             (if (and (not (null? stack)) (equal? (car stack) 'if))
                 parsed
                 (exit "body2")))
            ((member word '(define end)) (exit "body3"))
            (else (loop (cdr program) (push parsed word) stack))))
        parsed)))

; TESTS

(display "TEST 1") (newline)
(parse #(1 2 +))
(newline)

(display "TEST 2") (newline)
(parse #(x dup 0 swap if drop -1 endif)) 
(newline)

(display "TEST 3") (newline)
(parse #(define abs 
          dup 0 < 
          if neg endif 
          end 
          9 abs 
          -9 abs))
(newline)

(display "TEST 4") (newline)
(parse #( define -- 1 - end 
           define =0? dup 0 = end 
           define =1? dup 1 = end 
           define factorial 
           =0? if drop 1 exit endif 
           =1? if drop 1 exit endif 
           dup -- 
           factorial 
           * 
           end 
           0 factorial 
           1 factorial 
           2 factorial 
           3 factorial 
           4 factorial ))
(newline)

(display "TEST 5") (newline)
(parse #(define =0? dup 0 = end 
          define <0? dup 0 < end 
          define signum 
          =0? if exit endif 
          <0? if drop -1 exit endif 
          drop 
          1 
          end 
          0 signum 
          -2 signum ))
(newline)

(display "TEST 6") (newline)
(parse #(define word w1 w2 w3))
(newline)

(display "TEST 7") (newline)
(parse #(1 2 if + if dup - endif endif dup))
(newline)

(display "TEST 8") (newline)
(parse #(   define =0? dup 0 = end
             define =1? dup 1 = end
             define -- 1 - end
             define fib
             =0? if drop 0 exit endif
             =1? if drop 1 exit endif
             -- dup
             -- fib
             swap fib
             +
             end
             define make-fib
             dup 0 < if drop exit endif
             dup fib
             swap --
             make-fib
             end
             10 make-fib   ))

(display "TEST 9") (newline)
(parse #(display if end endif))
(newline)

(display "TEST 10") (newline)
(parse #(end display))
(newline)