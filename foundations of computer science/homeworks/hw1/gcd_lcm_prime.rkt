(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))


(define (prime? n)
  (define (inner_prime? n d)
    (or (or (= n d) (< n 2)) (and (not (= (modulo n d) 0)) (inner_prime? n (+ d 1)))))
  (inner_prime? n 2))
