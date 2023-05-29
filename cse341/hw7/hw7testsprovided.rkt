#lang racket

(require "hw7.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.
(require rackunit)

; Note tests will fail until you implement appropriate aspects of your Racket solution.

; use this for comparing two geometry-values because Racket's equal? requires
; reference equality for objects (which is reasonable since in general state is mutable)
; Note: This approach does not work for non-value geometry expressions because they do not
;       have getters defined. That makes testing (only) preprocessing of non-value
;       geometry expressions difficult.  You could add getters for debugging if needed.
(define (same-geometry-value v1 v2)
  (or (and (is-a? v1 no-points%)
           (is-a? v2 no-points%))
      (and (is-a? v1 point%)
           (is-a? v2 point%)
           (= (send v1 get-x) (send v2 get-x))
           (= (send v1 get-y) (send v2 get-y)))
      (and (is-a? v1 line%)
           (is-a? v2 line%)
           (= (send v1 get-m) (send v2 get-m))
           (= (send v1 get-b) (send v2 get-b)))
      (and (is-a? v1 vertical-line%)
           (is-a? v2 vertical-line%)
           (= (send v1 get-x) (send v2 get-x)))
      (and (is-a? v1 line-segment%)
           (is-a? v2 line-segment%)
           (= (send v1 get-x1) (send v2 get-x1))
           (= (send v1 get-y1) (send v2 get-y1))
           (= (send v1 get-x2) (send v2 get-x2))
           (= (send v1 get-y2) (send v2 get-y2)))))
      
(define (run-prog e)
  (send (send e preprocess-prog) eval-prog null))

(define (check-prog e ans s)
  (check-true (same-geometry-value (run-prog e) ans) s))

(define tests
  (test-suite
   "Homework 7 Tests"

   (check-true (same-geometry-value
                (send (new line-segment% [x1 3.2][y1 4.1][x2 3.2][y2 4.1]) preprocess-prog)
                (new point% [x 3.2][y 4.1]))
               "convert a line-segment% to a point%")

   (check-true (same-geometry-value
                (send (new line-segment% [x1 -3.2][y1 -4.1][x2 3.2][y2 4.1]) preprocess-prog)
                (new line-segment% [x1 3.2][y1 4.1][x2 -3.2][y2 -4.1]))
               "flip an improper line-segment%")

   (check-prog (new shift% [dx 3.0][dy 4.0][e (new point% [x 4.0][y 4.0])])
               (new point% [x 7.0][y 8.0])
               "shifting a point")

   (check-prog (new let% [s "a"][e1 (new point% [x 4.0][y 4.0])]
                         [e2 (new shift% [dx 3.0][dy 4.0][e (new var% [s "a"])])])
               (new point% [x 7.0][y 8.0])
               "using a variable")

   (check-prog (new let% [s "a"][e1 (new point% [x 1.0][y 1.0])]
                         [e2 (new let% [s "a"][e1 (new point% [x 4.0][y 4.0])]
                                       [e2 (new shift% [dx 3.0][dy 4.0][e (new var% [s "a"])])])])
               (new point% [x 7.0][y 8.0])
               "using a shadowing variable")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 0.0][y2 2.0])]
                               [e2 (new line-segment% [x1 0.0][y1 1.0][x2 0.0][y2 3.0])])
               (new line-segment% [x1 0.0][y1 2.0][x2 0.0][y2 1.0])
               "intersection: vertical segments overlapping")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 0.0][y2 4.0])]
                               [e2 (new line-segment% [x1 0.0][y1 1.0][x2 0.0][y2 3.0])])
               (new line-segment% [x1 0.0][y1 3.0][x2 0.0][y2 1.0])
               "intersection: vertical segment containment")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 0.0][y2 4.0])]
                               [e2 (new line-segment% [x1 0.0][y1 10.0][x2 0.0][y2 13.0])])
               (new no-points%)
               "intersection: vertical segments no intersection")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 0.0][y2 4.0])]
                               [e2 (new line-segment% [x1 0.0][y1 4.0][x2 0.0][y2 5.0])])
               (new point% [x 0.0][y 4.0])
               "intersection: vertical segments just touching")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 2.0][y2 4.0])]
                               [e2 (new line-segment% [x1 1.0][y1 2.0][x2 3.0][y2 6.0])])
               (new line-segment% [x1 2.0][y1 4.0][x2 1.0][y2 2.0])
               "intersection: overlapping non-vertical segments")

   (check-prog (new intersect% [e1 (new line-segment% [x1 0.0][y1 0.0][x2 3.0][y2 6.0])]
                               [e2 (new line-segment% [x1 1.0][y1 2.0][x2 2.0][y2 4.0])])
               (new line-segment% [x1 2.0][y1 4.0][x2 1.0][y2 2.0])
               "intersection: non-vertical segment containment")

   (check-prog (new intersect% [e1 (new line-segment% [x1 1.0][y1 2.0][x2 2.0][y2 4.0])]
                               [e2 (new line-segment% [x1 0.0][y1 0.0][x2 3.0][y2 6.0])])
               (new line-segment% [x1 2.0][y1 4.0][x2 1.0][y2 2.0])
               "intersection: non-vertical segment containment, reversed order")))

(require rackunit/text-ui)

;; runs the test
(run-tests tests)
