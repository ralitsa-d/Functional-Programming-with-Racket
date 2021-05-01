#lang racket
;lecture 1
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))

(define (suma n) (if (= n 0) 0 (+ (/ n (+ (* n n) 1)) (suma (- n 1)))))

;lecture 2
(define (fact n)
  (define (fact-helper product count maxcount) (if (> count maxcount) product (* count (fact-helper product (+ count 1) maxcount))))
  (fact-helper 1 1 n))

;; най-голямото цяло число, което дели двете числа без остатък
(define (mygcd u v)
       (cond[(= u 0) v]
            [(= v 0) u]
            [(and (> u v) (> v 0) (> u 0)) (mygcd v (remainder u v))]
            [(and (> v u) (> v 0) (> u 0)) (mygcd u (remainder v u))]))

(define (user-gcd u v)
  (define (pos-gcd u v)
    (if (> u v) (bas-gcd u v) (bas-gcd v u)))
  (define (bas-gcd u v) (cond[(= v 0) u]
                             [else (bas-gcd v (remainder u v))]))
  (cond[(= u 0) (abs v)]
       [(= v 0) (abs u)]
       [else (pos-gcd (abs u) (abs v))])
  )
;; числата на фибоначи - дървовидна рекурсия
(define (fib n)
  (cond[(= n 0) 0]
       [(= n 1) 1]
       [else (+ (fib (- n 1)) (fib (- n 2)))]))
;; по колко начина определене сума пари(в стотинки) може да се "развали". kinds = 6 означава, че искаме да разваляме с до 50 ст.
(define (count-change amount)
  (define (helper amount kinds) (cond[(= amount 0) 1]
                                     [(< amount 0) 0]
                                     [(and (= kinds 0) (> amount 0)) 0]
                                     [else (+ (helper amount (- kinds 1)) (helper (- amount (denom kinds)) kinds))]))
  (define (denom kinds) (cond[(= kinds 1) 1]
                             [(= kinds 2) 2]
                             [(= kinds 3) 5]
                             [(= kinds 4) 10]
                             [(= kinds 5) 20]
                             [(= kinds 6) 50]))
  (helper amount 6))

;; увод за функциите от по-висок ред
(define (sum-int a b) (if (> a b) 0 (+ a (sum-int (+ a 1) b))))

;; sum of cubes of numbers from a to b
(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (if (> a b) 0 (+ (cube a) (sum-cubes (+ a 1) b))))

;;lecture 3
;;letrec
(define (fact_ n)
  (letrec ([fact-iter (lambda (x y) (if (= x 0) y (fact-iter (- x 1) (* x y))))])
    (fact-iter n 1)))

;; ??!?
(define (even-odd? n)
  (letrec ([even? (lambda (x) (or (= x 0) (even? (- x 1)))) ]
           [odd? (lambda (x) (and (not (= x 0)) (even? (- x 1))))])
    (odd? n)))

;;връщаме функция, която да е приложена n пъти
(define (repeated f n) (lambda (x) (if (= n 1) (f x) (f ((repeated f (- n 1)) x) ) )))

(define square (lambda (x) (* x x))) 

;lecture 4
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(define (new-cons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1" m)]))
  dispatch) 
 
(define (new-car z) (z 0)) 
 
(define (new-cdr z) (z 1)) 

;;lecture 5
(define (atom? a) (not (pair? a)))

(define (count-atoms l) (cond[(null? l) 0]
                           [(atom? l) 1]
                           [else (+ (count-atoms (car l)) (count-atoms (cdr l)))]))


;reverse & deep-reverse
(define (rev lst) (if (null? lst) '() (append (rev (cdr lst)) (list (car lst)))))

(define (deep-rev lst) (cond[(null? lst) '()]
                            [(atom? lst) lst]
                            [else (append (deep-rev (cdr lst)) (list (deep-rev (car lst))))]))

(define (prod lst) (cond[(null? lst) 1]
                        [else (* (car lst) (prod (cdr lst)))]))

;map
;(define (map f lst) (cond[(null? lst) lst]
;                         [else (cons (f (car lst)) (map f (cdr lst)))]))


;filter
(define (filt pred lst) (cond[(null? lst) lst]
                             [(pred (car lst)) (cons (car lst) (filt pred (cdr lst)))]
                             [else (filt pred (cdr lst))]))

(define (filtt pred lst)
  (define (helper arg acc) (cond[(null? arg) acc]
                                [(pred (car arg)) (helper (cdr arg) (append acc (list (car arg))))]
                                [else (helper (cdr arg) acc)]))
  (helper lst '()))


;namirane na sbora na elementite, koito preminawat prez daden filter
(define (sum-filter pred lst)
  (apply + (map (lambda(x) (if (pred x) x 0 )) lst)))

; eval ot lekciqta... no ne raboti 
;(eval (cons '+ '(1 2 3))) 

;lecture 6
;a-lists
(define (remove-assoc key a-list) (cond[(null? a-list) a-list]
                                      [(eq? key (caar a-list)) (cdr a-list)]
                                      [else (cons (car a-list) (remove-assoc key (cdr a-list)))]))

(define (put-assoc pair a-list) (cons pair a-list))

;trees
(define tree '((a b c) (b d) (c f g h) (g i j)))

(define (succs node)
  (let ([lst (assq node tree)])
    (if lst (cdr lst) '())))

(define (leaf? node) (if (null? (succs node)) #t #f))

(define (list-leaves node) (if (leaf? node) (list node) (apply append (map list-leaves (succs node)))))

(define (paths node) (cond[(leaf? node) (list (list node))]
                          ;[else (apply append (map paths (succs node)))]))
                          [else (map (lambda (x) (cons node x))(apply append (map paths (succs node))))]))

;graph
(define graph '((a b d) (b c) (c b) (d c e) (e a f) (f)))

(define (succsG node)
  (let ([lst (assq node graph)])
    (if lst (cdr lst) '())))

(define (is-a-node node) (cond [(assq node graph) #t]
                               [else #f]))

(define (is-a-path lst) (cond[(null? lst) #t]
                             [(null? (cdr lst)) (is-a-node (car lst))]
                             [(memq (cadr lst) (succsG (car lst))) (is-a-path (cdr lst))]))

(define (cycled lst) (cond[(null? lst) #f]
                          [(memq (car lst) (cdr lst)) #t]
                          [else (cycled (cdr lst))]))

(define (correct-path lst) (and (is-a-path lst) (not (cycled lst))))

(define (gen-next-path path) (map (lambda (x) (cons x path)) (succsG (car path))))

(define (generate-paths lst) (apply append (map gen-next-path lst)))

(define (connected a b n) (cond[(eq? a b) #t]
                               [(<= n 0) #f]
                               [else (connect1 (list (list a)) b n)]))

(define (connect1 lst b n) (cond[(<= n 0) #f]
                                [(null? lst) #f]
                                [else (let ([genp (generate-paths lst)])
                                        (if (assq b genp) #t (connect1 genp b (- n 1))))]))

;lecture 7
(define new-withdraw (let([balance 100]) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance)  (error "Not possible")))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance) "Not possible"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
 (define (dispatch m);;също като cin>>
   (cond [(eq? m 'withdraw) withdraw]
         [(eq? m 'deposit) deposit]
         [else (error "Unknown request!" m)]))
  dispatch)

(define acc (make-account 100))
;((acc 'deposit) 100)


;lecture 8
;;създава стрийм, но не го принтира
(define (stream-enum low high)
  (if (> low high) empty-stream (stream-cons low (stream-enum (+ low 1) high)))) 


;(stream-first (stream-enum 3 6))
;(stream-rest (stream-enum 2 9))

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

(define (second-prime)
  (stream-first (stream-rest (stream-filter prime? (stream-enum 1000 10000)))))


(define (merge-streams s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else (let ([h1 (stream-first s1)]
                    [h2 (stream-first s2)])
                (cond [(< h1 h2)
                       (stream-cons h1
                                    (merge-streams
                                     (stream-rest s1) s2))]
                      [(> h1 h2)
                       (stream-cons h2 (merge-streams s1 (stream-rest s2)))]
                      [else (stream-cons h1 (merge-streams (stream-rest s1) (stream-rest s2)))]))])) 



;exercises

(define (ordered? integer) (cond[(< integer 10) #t]
                                [(and (< integer 100) (>= (remainder integer 10) (remainder (quotient integer 10) 10))) #t]
                                [else (cond[(and (<= (remainder (quotient integer 10) 10) (remainder integer 10)) (ordered? (quotient integer 10))) #t]
                                           [else #f])]))

(define (ordered-by? pred integer) (cond[(< integer 10) #t]
                                [(and (< integer 100) (pred (remainder integer 10) (remainder (quotient integer 10) 10))) #t]
                                [else (cond[(and (pred (remainder integer 10) (remainder (quotient integer 10) 10)) (ordered-by? pred (quotient integer 10))) #t]
                                           [else #f])]))


(define (how-many n lst) (cond[(null? lst) 0]
                              [(= (car lst) n) (+ 1 (how-many n (cdr lst)))]
                              [(not (= (car lst) n)) (how-many n (cdr lst))]))


;(define (omg-wut-doooo .wut)
;   wut)

; (omg-wut-doooo 1)
; (omg-wut-doooo 1 2 3)

;(define (prepend lst1 lst2) (append lst1 lst2))

(define (prepend l1 l2) (cond[(null? l2) l1]
                             [else (cons (car l2) (prepend l1 (cdr l2)))]))

(define (construct func lst) (cond[(null? lst) lst]
                                  [else (cons (func (car lst)) (construct func (cdr lst)))]))

(define (remove-dupes lst)
  (define (helper res l) (cond[(null? l) (reverse res)]
                              [(member (car l) res) (helper res (cdr l))]
                              [else (helper (cons (car l) res) (cdr l))]))
  (helper '() lst))

(define (longest str)
  (define (longer-word? lst1 lst2) (cond[(> (length lst1) (length lst2)) lst1]
                                        [else lst2]))
  (define (break-char? c) (cond[(or (char-whitespace? c) (char-punctuation? c)) #t]
                               [else #f]))
  (define (helper xs longest-curr current) (cond[(null? xs) (list->string (reverse (longer-word? longest-curr current)))]
                                                [(break-char? (car xs)) (helper (cdr xs) (longer-word? longest-curr current) null)]
                                                [else (helper (cdr xs) longest-curr (cons (car xs) current))]))
  (helper (string->list str) null null))


(define (sum-odd lst) (apply + (filter odd? lst)))


(define (dot-product lst1 lst2) (apply + (map (lambda(x y) (* x y)) lst1 lst2)))

(define (replicate n x) (map (lambda (_) x) (range 0 n)))

(define (maxi lst curr) (cond[(null? lst) curr]
                             [(null? curr) (maxi lst (- (car lst) 1))]
                             [(> (car lst) curr) (maxi (cdr lst) (car lst))]
                             [(<= (car lst) curr) (maxi (cdr lst) curr)]))


(define (better lst) (maxi (map (lambda (word) (apply + (map (lambda (ch) (char->integer ch)) (string->list word)))) lst) null))

(define (mapcat f l) (apply append (map f l)))
;(mapcat (lambda (x) (map (lambda (y) (+ 1 y)) x)) '((1 2) (3 4) (5)))

(define (h f g) (lambda (x) (max (f x) (g x))))
;((h (lambda (a) (+ a 1)) (lambda (b) (+ 2 b))) 5)

;graph
(define edges '((a b) (a d) (d c) (c f) (a f)))
(define (allNodes) (remove-duplicates (apply append (map car edges) (map cdr edges))))
;(allNodes)

(define (pairs lst)
  (define (count-larger-than x) (length (filter (lambda(a) (> a x)) lst)))
  (map (lambda(x) (cons x (count-larger-than x))) lst))
;(pairs '(1 2 3))


(define (generate-alphabet-checker alphabet)
  (define (in-alphabet? word)
    (andmap (lambda (ch)
              (equal? #t (member ch alphabet)))
            (string->list word)))
  (lambda (words)
    (andmap in-alphabet? words)))

;((generate-alphabet-checker '(#\a #\b #\c #\d #\e)) '("abc" "cde"))


(define (allTheSame lst1 lst2) (andmap (lambda(x y) (and (not(equal? #f(member x lst2))) (not(equal? #f (member y lst1))))) lst1 lst2))
;(allTheSame '("car" "array" "mo") '("mo" "array" "car"))

(define (matrix? lst)
  (let ([len (length (car lst))])
    (andmap (lambda(x) (eqv? len (length x))) lst)))
;(matrix? '((1 2 3) (4 5 6) (7 8 9)))


;THE END












  