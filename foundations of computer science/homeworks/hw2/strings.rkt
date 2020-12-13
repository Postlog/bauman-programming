(define (string-reverse str)
  (list->string (reverse (string->list str))))

(define (string-cdr str)
  (list->string (cdr (string->list str))))

(define (string-trim-left str)
  (define (inner lstr)
    (if (null? lstr)
        ""
        (if (char-whitespace? (car lstr))
            (inner (cdr lstr))
            (list->string lstr))))
  (inner (string->list str)))

(define (string-trim-right str)
  (string-reverse (string-trim-left (string-reverse str))))

(define (string-trim str)
  (string-trim-right (string-trim-left str)))

(define (string-prefix? pref str)
  (define (inner lpref lstr)
    (if (or (null? lpref) (null? lstr))
        (null? lpref)
        (and (equal? (car lpref) (car lstr))
             (inner (cdr lpref) (cdr lstr)))))
  
  (inner (string->list pref) (string->list str)))

(define (string-suffix? suf str)
  (string-prefix? (string-reverse suf) (string-reverse str)))

(define (string-infix? inf str)
  (and (not (null? (string->list str)))
       (or (string-prefix? inf str)
           (string-infix? inf (string-cdr str)))))

(define (string-remove-prefix pref str)
  (define (inner lpref lstr)
    (if (or (null? lpref) (null? lstr))
        (list->string lstr)
        (inner (cdr lpref) (cdr lstr))))
  
  (inner (string->list pref) (string->list str)))

(define (string-split str del)
  (define (inner lstr acc)
    (if (null? lstr)
        (list (list->string acc))
        (if (string-prefix? del (list->string lstr))
            (cons (list->string acc)
                  (inner (string->list (string-remove-prefix del (list->string lstr)))
                         '()))
            (inner (cdr lstr) (append acc (list (car lstr)))))))
  (inner (string->list str) '()))
                                                           
 
