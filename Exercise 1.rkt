#lang racket
(define (sum-of-greater-primes n k)
  
  
  (define (prime? p)
    (define max (floor(sqrt p)))
    (define (divisible? a b)
      (cond[(= (remainder a b) 0) #t]
           [else #f]))
    (define (prime-helper? iter)
      (cond [(= p 3) #t]
        [(divisible? p iter) #f]
           [(= iter 2) (not (divisible? p iter))]
           [else (prime-helper? (- iter 1))])
    )
    (and (> p 1) (prime-helper? max))
    ;(prime-helper? max)
  )

  (define start (+ k 1))

  (define (sum-helper sum current-n current)
    (cond [(> current-n n) sum]
          [(prime? current) (sum-helper (+ sum current) (+ current-n 1) (+ current 1))]
          [else (sum-helper sum current-n (+ current 1))])
  )

  (sum-helper 0 1 (+ 1 k))
)
(sum-of-greater-primes 3 2)

;(define (make-list xss int count res)
;  (cond[(< count int) (make-list (list (map (lambda (x) (cdr x)) xss)) int (+ 1 count) res)]
;       [(eq? count int) (map (lambda (x) (list (append (list (res)) (list (car x))))) xss)]))
;(make-list '((1 2 3) (4 5 6) (7 8 9)) 2 1 '())

;(define (col xss n)
;  (andmap (lambda (x) (equal? (remove-duplicates x) x))
;          (list apply append (make-list xss n 1 '())))