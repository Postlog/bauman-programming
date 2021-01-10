(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define-syntax define-data
  (syntax-rules ()
    ((define-data data-name ((name field1 ...) ...))
     (begin
       (my-eval (list 'define
                      'name
                      (lambda (field1 ...)
                        (list (list 'd-name 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...)))) ...
       (my-eval (list 'define
                      (s->s (string-append (s->s 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2)
                             (let ((d-nameres (assoc 'd-name x)))
                               (and d-nameres (equal? (cadr d-nameres) 'data-name)))))))))))

(define-syntax match
  (syntax-rules ()
    ((match x ((name field1 ...) expr) ...)
       (cond
         ((equal? (cadadr x) 'name)
           (let ((field1 (cadr (assoc 'field1 x))) ...)
             expr))
          ...
          (else x)))))


; TESTS

; Определяем тип
;
(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

; Определяем значения типа
;
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

; Пусть определение алгебраического типа вводит
; не только конструкторы, но и предикат этого типа:
;
(display (and (figure? s)
              (figure? r)
              (figure? t)
              (figure? c))) (newline)

(define pi (acos -1)) ; Для окружности
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(display (perim s)) (newline)
(display (perim r)) (newline)
(display (perim t)) (newline)
(display (perim c)) (newline)