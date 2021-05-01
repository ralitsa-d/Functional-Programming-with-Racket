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











