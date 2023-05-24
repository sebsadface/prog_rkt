#lang racket

(require rackunit)
(require "hw6provided.rkt")
(require "hw6assignment.rkt")
(require racket/gui)
(require racket/vector)

(require rackunit)

; testing the `on-char`
(define (test-on-char)
  (define tetris-instance (new my-tetris%))
  (let ([board-instance (send tetris-instance get-board)])

    ; testing the 'u' key press
    (let ([initial-piece (send board-instance get-current-piece)])
      (send tetris-instance on-char (make-object key-event% 'press #\u))
      (check-equal? (send board-instance get-current-piece) initial-piece)
      (send tetris-instance on-char (make-object key-event% 'press #\u))
      (check-equal? (send board-instance get-current-piece) initial-piece))

    ; testing the 'c' key press
    (send board-instance set-score! 150)
    (send tetris-instance on-char (make-object key-event% 'press #\c))
    (equal? (send board-instance get-score) 150)))


; testing the `select-shape` function in the `my-board%` class
(define (test-select-shape)
  (define board-instance (new my-board% [game (new my-tetris%)]))

  ; testing when cheat is set to false
  (send board-instance set-cheat! #f)
  (equal? '(1) (vector-member (send board-instance select-shape) all-shapes))

  ; testing when cheat is set to true
  (send board-instance set-cheat! #t)
  (check-equal? (send board-instance select-shape) (vector (vector '(0 . 0)))))


; testing store-current
(define (test-store-current)
  (define board-instance (new my-board% [game (new my-tetris%)]))
  (let ([initial-grid (vector-copy (send board-instance get-grid))])
    (send board-instance store-current)
    (equal? (send board-instance get-grid) initial-grid)))


; running the tests
(test-on-char)
(test-select-shape)
(test-store-current)
