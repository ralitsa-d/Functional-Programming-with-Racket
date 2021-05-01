#lang racket
; първа задача
(define (arcsin z)
  
  (define (doubleFact p) 
   (cond [(= p 0) 0]
         [(= p 1) 1]
         [(= p 2) 2]
         [(= p 3) 3]
         [(> p 3) (* p (doubleFact (- p 2)))]
   )
  )
  
  (define (res n z) (* (/ (doubleFact (- (* 2 n) 1)) (doubleFact(* 2 n)) ) (/ (expt z (+ 1 (* 2 n))) (+ 1 (* 2 n)))))
  
 
    (define (sum-iter result counter)
      (cond [(< result 1E-6) result]
            [else sum-iter (+ result (res counter z)) (+ counter 1)]))
  
  (sum-iter 1 1)
)



; втора задача
(define (automorphic? n)
  (define n2 (expt n 2))
  (define (automorphic-helper? currentn2 currentn)
    (cond[(not (= currentn2 currentn)) #f]
         [(and (< currentn 10) (= currentn2 currentn)) #t]
         [else (automorphic-helper? (remainder (quotient n2 10) 10) (remainder (quotient n 10) 10))])
  )
  (automorphic-helper? (remainder n2 10) (remainder n 10))
)

(automorphic? 6)

; трета задача
(define (sum-of-greater-primes n k)
  
  
  (define (prime? p)
    (define max (floor(sqrt p)))
    (define (divisible? a b)
      (cond[(= (remainder a b) 0) #t]
           [else #f]))
    (define (prime-helper? iter)
      (cond[(= p 3) #t]
            [(= p 2) #f]
        [(and (divisible? p iter) (> iter 2)) #f]
           [(= iter 2) (not (divisible? p iter))]
           [else (prime-helper? (- iter 1))])
    )
    (and (> p 1) (prime-helper? max))
  )

  (define start (+ k 1))

  (define (sum-helper sum current-n current)
    (cond [(> current-n n) sum]
          [(prime? current) (sum-helper (+ sum current) (+ current-n 1) (+ current 1))]
          [else (sum-helper sum current-n (+ current 1))])
  )

  (sum-helper 0 1 (+ 1 k))
)

(sum-of-greater-primes 1 5)