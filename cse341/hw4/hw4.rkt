
#lang racket

;; This line exports all of your defined functions,
;; so you can call them in hw4tests.rkt and so we can
;; call them in our tests.
;; Don't remove or comment out this line!
(provide (all-defined-out)) 

;; Implement your code below

;; #1
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

;; keep going with the rest of the problems below

;;#2
(define (string-append-map xs suffix)
  (map (λ (x) (string-append x suffix)) xs))

;;#3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;;#4
(define (stream-first-k-such-that f k s)
  (if (<= k 0)
      null
      (if (f (car (s)))
          (cons (car (s)) (stream-first-k-such-that f (- k 1) (cdr (s))))
          (stream-first-k-such-that f (- k 1) (cdr (s))))))

;;#5
(define funny-number-stream
  (letrec ([f (λ (x) (cons x (λ ()
                               (if (< x 0)
                                   (f (+ (- x) 1))
                                   (if (= (remainder (+ x 1) 6) 0)
                                       (f (- 0 (+ x 1)))
                                       (f (+ x 1)))))))])
    (λ () (f 1))))

;;#6
(define dan-then-dog
  (letrec ([f (λ (x y) (cons x (λ () (f y x))))])
    (λ () (f "dan.jpg" "dog.jpg"))))

;;#7
(define (stream-add-one s)
  (letrec ([f (λ (s) (cons (cons 1 (car (s))) (λ () (f (cdr (s))))))])
    (λ () (f s))))

;;#8
(define (cycle-lists xs ys)
  (letrec ([f (λ (xs ys n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (λ () (f xs ys (+ n 1)))))])
    (λ () (f xs ys 0))))

;;#9
(define (vector-assoc v vec)
  (letrec ([f (λ (v vec n)
                (cond [(= n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n))
                              (if (= v (car (vector-ref vec n)))
                                  (vector-ref vec n)
                                  (f v vec (+ n 1)))]
                      [#t (f v vec (+ n 1))]))])
    (f v vec 0)))