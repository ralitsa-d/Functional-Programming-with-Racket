#lang racket
;;(define lol-list '(1 2 3));
;;(cons 4 lol-list);
;;(append lol-list 5);
;;(reverse lol-list);
;;(member 2 lol-list);
;;(eq? null '());




;;; Exercise 1 
(define (prepend '(c d) (list a b))
  (cond [((member c list) && (member d list)) list]
        [else (cons c list) (cons d list)]);
)

;(prepend '(3 4) (list 1 2));



;;(define (prepend-list xs xy)
;;  (cond [(null? xy) xs]
;;;        [else (cons (car xy) (prepend-list xs (cdr xy)))])
;;)
;;(prepend-list '(3 4) (list 1 2));



;(define (prepend-list xs xy)
 ; (cond [(null? xy) xs]
  ;      [else (cons (car xy) (prepend-list))])
;)






;;Exercise 2

;(define (apply-to-list f xs)
 ; (cond [(null? xs) null]
  ;      [else (cons (f (car xy)) (appply-to-list (cdr xy) f))])
  ;)

;; Exercise 3

;(define (remove-dupes '(xs))
  ;(define new-list '())
 ; (cond [(null? xs) null]
   ;     [else
    ;     (cond [(member (car xs) (cdr xs)) (prepend '() (cdr xs))]
     ;          [else (remove-dupes (cdr xs))])])
;)
;(remove-dupes '(1 1 2 2 3 3));

(define (longer l1 l2)
  (cond [(> (length l1)(length l2)) l1]
        [else l2]))

(define (break_char? c)
  (or (char-whitespace? c) (char-punctuation? c)))

(define (longest-word str)
  (define (helper str max curr)
    (cond [(break_char? (car str) (helper (cdr str) max (car str)))]))
)