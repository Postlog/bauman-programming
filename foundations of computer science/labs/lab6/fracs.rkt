(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define (dict-ref dict key)
  (cadr (assoc key dict)))

(define (last xs)
  (list-ref xs (- (length xs) 1)))

(define (numeric? char)
  (let ((code (char->integer char)))
    (and (> code 47) (< code 58))))

(define (get-sign str)
  (if (equal? (car str) #\-)
      '-
      '+))

(define (trim-sign str)
  (if (or (equal? (car str) #\-)
          (equal? (car str) #\+))
      (cdr str)
      str))

(define (token->number token)
  (string->number (list->string token)))

(define (tokenize-frac lstr)
  (let ((tokens (list (list 'sign (get-sign lstr)) (list 'numerator '()) (list 'denominator '()))))
    (let tokenize-loop ((lstr (trim-sign lstr)) (stage 'numerator))
      (if (not (null? lstr))
          (let ((char (car lstr)) (other (cdr lstr)))
            (if (equal? char #\/)
                (if (equal? stage 'numerator)
                    (tokenize-loop other 'denominator))
                (if (numeric? char)
                    (begin (set-car! (cdr (assoc stage tokens)) (append (cadr (assoc stage tokens)) (list char)))
                           (tokenize-loop other stage))
                    (set-car! (cdr (assoc stage tokens)) '()))))))
    tokens))

(define (check-frac str)
  (let ((tokens (tokenize-frac (string->list str))))
    
    (and (not (null? (dict-ref tokens 'numerator)))
         (not (null? (dict-ref tokens 'denominator))))))

(define (scan-frac str)
  (and (check-frac str)
       (let ((tokens (tokenize-frac (string->list str))))
         (my-eval (list '/
                        (list (dict-ref tokens 'sign)
                              (token->number (dict-ref tokens 'numerator)))
                        (token->number (dict-ref tokens 'denominator)))))))

(define (split-fracs str)
  (let loop ((fracs '()) (it-frac "") (lstr (string->list str)))
    (if (null? lstr)
        (if (> (string-length it-frac) 0)
            (append fracs (list it-frac))
            fracs)
        (if (or (equal? (car lstr) #\tab)
                (equal? (car lstr) #\newline)
                (equal? (car lstr) #\space))
            (if (> (string-length it-frac) 0)
                (loop (append fracs (list it-frac)) "" (cdr lstr))
                (loop fracs it-frac (cdr lstr)))
            (loop fracs (string-append it-frac (string (car lstr))) (cdr lstr))))))

(define (scan-many-fracs str)
  (define (inner str)
    (let loop ((str-fracs (split-fracs str)))
      (if (null? str-fracs) '()
          (let ((scanned-frac (scan-frac (car str-fracs))))
            (if scanned-frac
                (cons scanned-frac (loop (cdr str-fracs)))
                (list scanned-frac))))))
  (let ((scanned-fracs (inner str)))
    (and (last scanned-fracs) scanned-fracs)))
            
