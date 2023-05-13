
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
                              (if (equal? v (car (vector-ref vec n)))
                                  (vector-ref vec n)
                                  (f v vec (+ n 1)))]
                      [#t (f v vec (+ n 1))]))])
    (f v vec 0)))

;;#10
(define (caching-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next-pos 0]
           [f (λ (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! cache next-pos new-ans)
                              (set! next-pos (if (>= (+ next-pos 1) n) 0 (+ next-pos 1)))
                              new-ans) new-ans)))))])
    f))

;;#11
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (let ([x e1])
      (letrec ([f (λ (x y)
          (if (> y x)
              (f x e2)
              #t))])
      (f e1 e2)))]))
        
;;#12
(define (cycle-lists-challenge xs ys)
  (letrec ([f (λ (xs ys)
                (cons (cons (car xs) (car ys))
                      (λ () (f (if (null? (cdr xs)) xs (cdr xs))
                                (if (null? (cdr ys)) ys (cdr ys))))))])
    (λ () (f xs ys))))

;;#13
(define (caching-assoc-lru xs n)
  (letrec ([cache (make-vector n #f)]
           [timestamps (make-vector n 0)]
           [current-time 0]
           [f (λ (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      (begin
                        (vector-set! timestamps (vector-assoc ans cache) current-time)
                        (set! current-time (+ current-time 1))
                        ans)
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (let ([least-recently-used (let loop ([i 0] [min-i 0] [min-time (vector-ref timestamps 0)])
                                                           (if (< i n)
                                                               (let ([time (vector-ref timestamps i)])
                                                                 (if (< time min-time)
                                                                     (loop (+ i 1) i time)
                                                                     (loop (+ i 1) min-i min-time)))
                                                               min-i))])
                                (vector-set! cache least-recently-used new-ans)
                                (vector-set! timestamps least-recently-used current-time))
                              (set! current-time (+ current-time 1))
                              new-ans)
                            new-ans)))))])
    (λ (v) (f v))))
