(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define (my-eval exprs)
  (eval exprs (interaction-environment)))


(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (field1 ...))
     (begin
       (my-eval (list 'define
                      (s->s (string-append "make-" (s->s 'name)))
                      (lambda (field1 ...)
                        (list (list 'type 'name) (list 'field1 field1) ...))))
       (my-eval (list 'define
                      (s->s (string-append (s->s 'name) "?"))
                      (lambda (x)
                        (and (list? x) (not (null? x))
                             (let ((ares (assoc 'type x)))
                               (and ares (equal? (cadr ares) 'name)))))))
       (my-eval (list 'define
                       (s->s (string-append (s->s 'name) "-" (s->s 'field1)))
                       (lambda (x)
                         (cadr (assoc 'field1 (cdr x)))))) ...
       (my-eval (list 'define
                       (s->s (string-append "set-" (s->s 'name) "-" (s->s 'field1) "!"))
                       (lambda (x val)
                         (set-car! (cdr (assoc 'field1 (cdr x))) val)))) ... ))))
                               
                           
                               
                                                    
                         
       