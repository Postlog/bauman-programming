(define force-return 0)
(define (exit) (force-return #f))

(call-with-current-continuation
 (lambda (stack)
   (set! force-return stack)))

(define (push xs x)
  (append xs (list x)))

(define (head xs n)
  (if (= n 0)
      '()
      (cons (car xs) (head (cdr xs) (- n 1)))))


(define (alphabetic? char)
  (let ((code (char->integer char)))
    (or (and (>= code 97) (<= code 122))
        (and (>= code 65) (<= code 90)))))

(define (numeric? char)
  (let ((code (char->integer char)))
    (and (>= code 48) (<= code 57))))
  
(define (tokenize str)
  (define operators '(#\- #\+ #\/ #\* #\^))

  (define (variable-name-end-index lstr)
    (let loop ((lstr lstr) (index 0))
      (if (or (null? lstr) (not (alphabetic? (car lstr))))
          index
          (loop (cdr lstr) (+ 1 index)))))

  (define (numeric-constant-end-index lstr)
    (let loop ((lstr lstr) (index 0) (prev-char #f))
      (if (null? lstr) index
          (let ((char (car lstr)))
            (if (or (numeric? char) (member char '(#\e #\.))
                    (and (member char '(#\+ #\-)) (equal? prev-char #\e)))
                (loop (cdr lstr) (+ index 1) char)
                index)))))
        
  (let loop ((lstr (string->list str)) (tokenized '()))
    (if (null? lstr)
        tokenized
        (let ((char (car lstr)))
          (cond
            ((member char operators) (loop (cdr lstr) (push tokenized (string->symbol (string char)))))
            ((member char '(#\( #\))) (loop (cdr lstr) (push tokenized (string char))))
            ((member char '(#\tab #\space #\newline)) (loop (cdr lstr) tokenized))
            ((alphabetic? char)
             (let ((index (variable-name-end-index lstr)))
               (loop (list-tail lstr index) (push tokenized (string->symbol (list->string (head lstr index)))))))
            ((numeric? char)
             (let ((index (numeric-constant-end-index lstr)))
               (loop (list-tail lstr index) (push tokenized (string->number (list->string (head lstr index)))))))
            (else (exit)))))))


(define (parse tokens)
  (define index 0)
  (define (inc) (set! index (+ 1 index)))
  (define (in-bounds?) (< index (length tokens)))
  (define (current) (list-ref tokens index))

  (define (only-close-brackets?)
    (let loop ((index index))
      (or (= index (length tokens))
          (and (equal? (list-ref tokens index) ")")
               (loop (+ index 1))))))

  (define (has-close-bracket?)
    (let loop ((index index) (offset 0))
      (if (= index (length tokens)) #f
          (let ((current (list-ref tokens index)))
            (cond
              ((and (equal? current ")") (zero? offset)))
              ((equal? current "(") (loop (+ index 1) (+ offset 1)))
              ((equal? current ")") (loop (+ index 1) (- offset 1)))
              (else (loop (+ index 1) offset)))))))
  
  (define (expression)
    (let loop ((T (term)))
      (if (and (in-bounds?) (or (equal? (current) '-) (equal? (current) '+)))
          (let ((op (current)))
            (inc)
            (if (not (in-bounds?)) (exit))
            (loop (list T op (term))))
          (if (not (only-close-brackets?)) (exit) T))))
        
  (define (term)
    (let loop ((F (factor)))
      (if (and (in-bounds?) (or (equal? (current) '/) (equal? (current) '*)))
          (let ((op (current)))
            (inc)
            (loop (list F op (factor))))
          F)))
          

  (define (factor)
    (let ((P (power)))
      (if (and (in-bounds?) (equal? (current) '^))
          (begin (inc) (list P '^ (factor)))
          P)))

  (define (power)
    (if (not (in-bounds?))(exit))
    (let ((current (current)))
      (inc)
      (cond
        ((equal? current '-) (list '- (power)))
        ((equal? current "(") (if (has-close-bracket?)
                                  (expression)
                                  (exit)))
        ((number? current) current)
        ((symbol? current) current)
        (else (exit)))))
            
  (expression))

(define (tree->scheme tree)
  (if (and (list? tree) (= (length tree) 3))
      (let ((a (car tree)) (op (cadr tree)) (b (caddr tree)))
        (let ((op (if (equal? op '^) 'expt op)))
          (list op (tree->scheme a) (tree->scheme b))))
      tree))
      
