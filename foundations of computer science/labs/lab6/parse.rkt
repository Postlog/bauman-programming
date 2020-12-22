(define (find-tail xs target)
  (if (equal? (car xs) target)
      (cdr xs)
      (find-tail (cdr xs) target)))

(define (tail-endif program)
  (let loop ((program program) (offset -1))
    (let ((word (car program)))
      (cond
        ((and (equal? word 'endif) (zero? offset)) (cdr program))
        ((equal? word 'endif) (loop (cdr program) (- 1 offset)))
        ((equal? word 'if) (loop (cdr program) (+ 1 offset)))
        (else (loop (cdr program) offset))))))

(define (push xs x)
  (append xs (list x)))

(define (head xss n)
  (if (or (= n -1) (null? xss))
      '()
      (cons (car xss) (head (cdr xss) (- n 1)))))

(define (count xs token)
  (let loop ((xs xs) (count 0))
    (if (null? xs)
        count
        (if (equal? (car xs) token)
            (loop (cdr xs) (+ count 1))
            (loop (cdr xs) count)))))

(define (parse program)
  (let ((program (vector->list program)))
    (and (check program) (list (parse-articles (get-articles-section program)) (parse-body (get-body-section program))))))

(define (check program)
  (define (check-articles-names-exist program)
    (let loop ((articles (get-articles-section program)))
      (or (null? articles) (and (not (equal? (cadr articles) 'end)) (loop (find-tail articles 'end))))))
  (not (cond
         ((not (= (count program 'define) (count program 'end))))
         ((not (= (count program 'if) (count program 'endif))))
         ((not (check-articles-names-exist program)))
         (else #f))))

(define (get-articles-section-last-index program)
  (let loop ((program program) (index -1) (it-index 0))
    (if (not (null? program))
        (if (equal? (car program) 'end)
            (loop (cdr program) it-index (+ 1 it-index))
            (loop (cdr program) index (+ 1 it-index)))
        index)))

(define (get-articles-section program)
  (head program (get-articles-section-last-index program)))

(define (get-body-section program)
  (let ((index (get-articles-section-last-index program)))
    (if (= index 0)
        program
        (list-tail program (+ 1 index)))))

(define (parse-articles program)
  (let loop ((program program))
    (if (not (null? program))
        (cons (parse-article program) (loop (find-tail program 'end)))
        '())))

(define (parse-article program)
  (list (cadr program) (parse-body (cddr program))))
            
(define (parse-body program)
  (let loop ((program program) (parsed '()))
    (if (not (null? program))
        (let ((word (car program)))
          (cond
            ((equal? word 'if) (loop (tail-endif program) (push parsed (list 'if (loop (cdr program) '())))))
            ((equal? word 'endif) parsed)
            ((equal? word 'end) parsed)
            (else (loop (cdr program) (push parsed word)))))
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