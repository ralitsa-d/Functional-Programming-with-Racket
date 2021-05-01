#lang racket
;zad1
(define (make-cons item lst)
  (map (lambda (x) (cons item x)) lst))

(define (cartesian-product xs ys)
  (map (lambda (x) (make-cons x ys)) xs))

(cartesian-product '(1 2) '(3 4 5))

;zad 2
(define (row list)
  (equal? (remove-duplicates list) list))

(define (transpose xss)
  (apply (curry map list) xss))

(define (latin-square? xss)
  (and (andmap (lambda (x) (row x)) xss) (andmap (lambda (x) (row x)) (transpose xss))))

(latin-square? '((1 2 3) (4 5 6) (1 8 9)))

;zad3
(define (compose list)
  (define (compose-helper lst res)
    (cond[(null? lst) res]
         [(equal? (length lst) 1) lst]
         [else (compose-helper (cdr(cdr lst)) (append (list res) (list (cons (car lst) (cadr lst)))))]))
  (compose-helper list '()))

(define (apply-fs list)
  (map (lambda (x) ((car x) (cdr x)) ) list))

(define (pair-compose fs)
  (apply + (apply-fs fs)))

(pair-compose '((lambda (x) (+ x x)) (lambda(y) (+ 1 y)) (lambda (z) (+ 2 z)) (lambda (zz) (+ 3 z)) ))





;;zad1
(define (cartesian-product xs ys)
  (define (cartesian-iter lst1 lst2)
    (cond [(or (null? lst1) (null? lst2)) '()]
          [else (map (lambda(x) (cons x (map identity lst2))) lst1)]))

  (cartesian-iter xs ys))
;(cartesian-product '(1 2) '(3 4))

;;zad2
(define (compare? lst1 lst2)
  (map (map (lambda (x y)
              (cond [(not (equal? x y)) #t]
                    [else #f]) lst1 lst2)) lst1))

(compare? '(2 3) '(1))

(map (lambda (x) (+ 1 x)) '());

(define (only-once? xs)
  (define (only-once-iter result list1 list2)
    (cond[(null? list1) result]
         [(null? list2) (only-once-iter #t (cdr list1) (list(append list2 (car list1))))]
         [else (only-once-iter (map (lambda (x y) (= x y)) list1 list2) (cdr list1) (list(append list2 (car list1))))]))

  (only-once-iter #t xs '()))

;(only-once? '(1 2 3))

;(define (latin-square? xss)
;  (define (latin-square-iter result)
;    (cond [(only-once? (map (lambda (x) x) xss)) (= result #t)]
;          [else (= result #f)]))
;)


;(map (lambda(x) x) '((1 2) (3 4) (4 5)))

;zad2
(define (row res list)
  (let (cond[(not (null? list)) ([item (car list)])]
            [else (cond[(null? list) res]
                       [else (row (and res (andmap (lambda (x) (not (equal? item x))) (cdr list))) (cdr list))])])))
    ;(cond[(null? list) res]
     ;    [else (row (and res (andmap (lambda (x) (not (equal? item x))) (cdr list))) (cdr list))])))
(row #t '(1 2 3 4))

;(define (latin-square? xss)
 ; )