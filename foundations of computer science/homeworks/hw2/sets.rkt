(define (contains? x xs)
  (and (not (null? xs))
       (or (equal? (car xs) x)
           (contains? x (cdr xs)))))

(define (list->set xs)
  (define (inner xs acc)
    (if (null? xs)
        acc
        (if (contains? (car xs) acc)
            (inner (cdr xs) acc)
            (inner (cdr xs) (cons (car xs) acc)))))
  
  (inner xs '()))

(define (set? xs)
  (define (inner xs acc)
    (or (null? xs)
        (and (not (contains? (car xs) acc))
             (inner (cdr xs) (cons (car xs) acc)))))
  
  (inner xs '()))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (contains? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

(define (difference xs ys)
  (if (null? xs)
      '()
      (if (contains? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))

(define (symmetric-difference xs ys)
  (define (inner u ist)
    (if (null? u)
        '()
        (if (contains? (car u) ist)
            (inner (cdr u) ist)
            (cons (car u) (inner (cdr u) ist)))))

  (inner (union xs ys) (intersection xs ys)))

(define (set-eq? xs ys)
  (define (inner xs ys)
    (or (null? xs)
        (and (contains? (car xs) ys)
             (inner (cdr xs) ys))))

  (and (= (length xs) (length ys)) (inner xs ys)))
    
