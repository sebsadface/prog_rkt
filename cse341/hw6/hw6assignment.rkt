#lang racket

;; This is the only file you turn in, so do not modify the other files as
;; part of your solution.

(require "hw6provided.rkt")
(require racket/random) 
(provide my-tetris%)
; Uncomment this line if you do the challenge problem
;(provide my-tetris-challenge%)

;; Edit the two classes below to implement your enhancements.

(define my-tetris%
  (class tetris%
    
    (super-new)
    
    (define/override (reset-board)
      (send this set-board! (new my-board% [game this])))

    (define/augment (on-char event)
      (define keycode (send event get-key-code))
      (match keycode
        [#\u (begin (send (send this get-board) rotate-clockwise)
                    (send (send this get-board) rotate-clockwise))]
        [#\c (if (>= (send (send this get-board) get-score) 100)
                 (begin (send (send this get-board) set-cheat! #t)
                        (send (send this get-board) next-piece)
                        (send (send this get-board) set-score! (- (send (send this get-board) get-score) 100))
                        (send (send this get-board) set-cheat! #f))
                 #f)]
        [_ (inner #t on-char event)]))
    
    ))

(define my-board%
  (class board%
    
    (define cheat #f)
    (define/public (set-cheat! c) (set! cheat c))
 
    (super-new)
    
    (define/override (select-shape)
      (if cheat
          (vector (vector '(0 . 0)))
          (random-ref (vector-append all-shapes
                                     (vector
                                      (rotations (vector '(0 . 0) '(-1 . 0) '(-1 . 1) '(0 . 1) '(1 . 0)))
                                      (rotations (vector '(0 . 0) '(-1 . 0) '(-2 . 0) '(1 . 0) '(2 . 0)))
                                      (rotations (vector '(0 . 0) '(0 . 1) '(1 . 0))))))))
                                 
    (define/override (store-current)
      (define points (send (send this get-current-piece) get-points))
      (for ([idx (in-range (vector-length points))])  
        (match-define (cons x y) (vector-ref points idx))
        (when (>= y 0)
          (vector-set! (vector-ref (send this get-grid) y) x (send (send this get-current-piece) get-color))))
      (send this remove-filled)
      (send this set-delay! (max (- (send this get-delay) 2) 80)))
   ))
