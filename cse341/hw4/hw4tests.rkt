#lang racket

(require "hw4.rkt")
(require rackunit)


;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

;;#1
(check-equal? (sequence 2 1 5) '(1 3 5))
(check-equal? (sequence 1 1 1) '(1))  
(check-equal? (sequence 3 5 1) '())   

;;#2
(check-equal? (string-append-map '("hello" "world") "!") '("hello!" "world!"))  
(check-equal? (string-append-map '() "!") '())                                 
(check-equal? (string-append-map '("hi") "") '("hi"))

;;#3
(check-equal? (list-nth-mod '(1 2 3 4 5) 6) 2)   
(check-equal? (list-nth-mod '(1 2 3 4 5) 0) 1)  
(check-equal? (list-nth-mod '(1) 0) 1)          


(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))
;;#4 & 5
(define funny-test45 (stream-first-k-such-that (lambda (x) #t) 16 funny-number-stream))

;;#4 & 6
(define funny-test46 (stream-first-k-such-that (lambda (x) #t) 16 dan-then-dog))

;;#8
; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

;;#7 & 6
; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))

;;#9
(check-equal? (vector-assoc 'b '#((a . 1) (b . 2) (c . 3))) '(b . 2))   
(check-equal? (vector-assoc 'd '#((a . 1) (b . 2) (c . 3))) #f)          
(check-equal? (vector-assoc 'a '#()) #f)                              

;;#10
(check-equal? ((caching-assoc '((a . 1) (b . 2) (c . 3)) 2) 'b) '(b . 2))  
(check-equal? ((caching-assoc '((a . 1) (b . 2) (c . 3)) 2) 'd) #f)         
(check-equal? ((caching-assoc '() 2) 'a) #f)                                

;;#11
(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))

;;#12
(define (one-visual-test12)
  (place-repeatedly (open-window) 0.5 (cycle-lists-challenge nums files) 27))

;;#13
(check-equal? ((caching-assoc-lru '((a . 1) (b . 2) (c . 3)) 2) 'b) '(b . 2)) 
(check-equal? ((caching-assoc-lru '((a . 1) (b . 2) (c . 3)) 2) 'd) #f)        
(check-equal? ((caching-assoc-lru '() 2) 'a) #f)                               

