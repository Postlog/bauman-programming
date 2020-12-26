(define mactions '(+ - * / mod))
(define bactions '(< > =))

(define (inc x) (+ x 1))

(define (contains? xs x)
  (and (not (null? xs)) (or (equal? x (car xs)) (contains? (cdr xs) x))))

(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define (find-word token program index)
  (if (equal? (vector-ref program index) token)
      index
      (find-word token program (inc index))))

(define (pass x) x)

(define (action-helper aliases action stack)
  (let ((aliased-action (assoc action aliases)))
    (if aliased-action
        (let ((aliased-action (cadr aliased-action)))
          (my-eval (list (car aliased-action)
                         (list (cadr aliased-action) (list 'quote stack))
                         (list (caddr aliased-action) (list 'quote stack)))))
        (my-eval (list action (cadr stack) (car stack))))))

(define (maction action stack)
  (define aliases (list (list 'mod '(remainder cadr car))))
  (cons (action-helper aliases action stack) (cddr stack)))

(define (baction action stack)
  (cons (if (action-helper '() action stack) -1 0) (cddr stack)))

(define (interpret program init-stack)
  (let interpreter ((index 0) (stack init-stack) (return-stack '()) (definitions '()))
    (if (= index (vector-length program))
        stack
        (let ((word (vector-ref program index)))
          (cond
            ((number? word) (interpreter (inc index) (cons word stack) return-stack definitions))
            ((contains? mactions word) (interpreter (inc index) (maction word stack) return-stack definitions))
            ((contains? bactions word) (interpreter (inc index) (baction word stack) return-stack definitions))
            ((equal? word 'and) (interpreter (inc index) (cons (if (and (= (car stack) -1) (= (cadr stack) -1)) -1 0) (cddr stack)) return-stack definitions))
            ((equal? word 'or) (interpreter (inc index) (cons (if (or (= (car stack) -1) (= (cadr stack) -1)) -1 0) (cddr stack)) return-stack definitions))
            ((equal? word 'neg) (interpreter (inc index) (cons (- (car stack)) (cdr stack)) return-stack definitions))
            ((equal? word 'not) (interpreter (inc index) (cons (if (= (car stack) 0) -1 0) (cdr stack)) return-stack definitions))
            ((equal? word 'drop) (interpreter (inc index) (cdr stack) return-stack definitions))
            ((equal? word 'swap) (interpreter (inc index) (append (list (cadr stack) (car stack)) (cddr stack)) return-stack definitions))
            ((equal? word 'dup) (interpreter (inc index) (cons (car stack) stack) return-stack definitions))
            ((equal? word 'over) (interpreter (inc index) (cons (cadr stack) stack) return-stack definitions))
            ((equal? word 'rot) (interpreter (inc index) (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) return-stack definitions))
            ((equal? word 'depth) (interpreter (inc index) (cons (length stack) stack) return-stack definitions))
            ((equal? word 'define) (interpreter (inc (find-word 'end program index)) stack return-stack
                                                (cons (list (vector-ref program (inc index)) (+ index 2)) definitions)))
            ((contains? '(end exit) word) (interpreter (car return-stack) stack (cdr return-stack) definitions))
            ((equal? word 'if) (interpreter (if (zero? (car stack)) (inc (find-word 'endif program index)) (inc index)) (cdr stack) return-stack definitions))
            ((equal? word 'endif) (interpreter (inc index) stack return-stack definitions))
            ((equal? word 'while) (if (zero? (car stack))
                                      (interpreter (inc (find-word 'endwhile program index)) stack return-stack definitions)
                                      (interpreter (inc index) stack (cons index return-stack) definitions)))
            ((equal? word 'endwhile) (interpreter (car return-stack) stack (cdr return-stack) definitions))
            ((equal? word 'do) (interpreter (inc index) stack (cons index return-stack) definitions))
            ((equal? word 'until) (interpreter (if (zero? (car stack)) (inc index) (car return-stack)) stack (cdr return-stack) definitions))
            ((equal? word 'for) (if (<= (car stack) (cadr stack))
                                    (interpreter (inc index) stack (cons index return-stack) definitions)
                                    (interpreter (inc (find-word 'endfor program index)) (cddr stack) return-stack definitions)))
            ((equal? word 'endfor) (interpreter (car return-stack) (cons (inc (car stack)) (cdr stack)) (cdr return-stack) definitions))
            (else (interpreter (cadr (assoc word definitions)) stack (cons (inc index) return-stack) definitions)))))))

; TESTS

(interpret #(   define abs 
                    dup 0 < 
                    if neg endif 
                end 
                 9 abs 
                -9 abs      ) (quote ()))

(interpret #(   define =0? dup 0 = end
define <0? dup 0 < end
define signum
=0? if exit endif
<0? if drop -1 exit endif
drop
1
end
0 signum
-5 signum
10 signum       ) (quote ()))

(interpret #(   define -- 1 - end
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
4 factorial     ) (quote ()))

(interpret #(   define =0? dup 0 = end
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
10 make-fib     ) (quote ()))

(interpret #(   define =0? dup 0 = end
define gcd
=0? if drop exit endif
swap over mod
gcd
end
90 99 gcd
234 8100 gcd    ) '())

(interpret #( 0 swap while dup rot + swap 1 - endwhile drop) '(10)) ; sum
(interpret #( 1 swap do dup rot * swap 1 - until drop) '(5)) ; factorial
(interpret #( if 2 1 < if 3 endif endif) '(-1))
(interpret #( if 1 2 < if 3 endif endif) '(-1))
(interpret #(10 0 for dup rot swap endfor) '())