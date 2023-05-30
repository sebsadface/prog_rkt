#lang racket

; CSE341, Programming Languages, Homework 7, hw7.rkt (see also OCaml code)

(provide (all-defined-out)) ;; so we can put tests in a second file

; a little language for 2D geometry objects

; each subclass of geometry-expression%, including subclasses of geometry-value%,
;  needs to respond to messages preprocess-prog and eval-prog

; each subclass of geometry-value% additionally needs:
;   * shift
;   * intersect, which uses the double-dispatch pattern
;   * intersect-point, intersect-line, and intersect-vertical-line for 
;       for being called by intersect of appropriate classes and doing
;       the correct intersection calculuation
;   * (We would need intersect-no-points and intersect-line-segment, but these
;      are provided by geometry-value% and should not be overridden.)
;   *  intersect-with-segment-as-line-result, which is used by 
;      intersect-line-segment as described in the assignment
; you can define other helper methods, but will not find much need to

; Note: geometry objects should be immutable: do not set! private state

; Note: For eval-prog, represent environments as lists of pairs as described in the assignment

(define epsilon 0.00001)

(define (float-close f1 f2)
  (< (abs (- f1 f2)) epsilon))

(define (float-close-point x1 y1 x2 y2)
  (and (float-close x1 x2) (float-close y1 y2)))

; do not change this class definition
; just as a matter of style, we make all expression classes a subclass of
; geometry-expression% or geometry-value%
(define geometry-expression%
  (class object%
    (super-new)))

; do not change methods in this class definition *except*
; you can add methods to it if you wish
(define geometry-value%
  (class geometry-expression%
    (super-new)

    ; two-points-to-line could return a line% or a vertical-line%
    (define (two-points-to-line x1 y1 x2 y2)
      (if (float-close x1 x2)
          (new vertical-line% [x x1])
          (let* ([m (/ (- y1 y2) (- x1 x2))]
                 [b (- y2 (* m x2))])
            (new line% [m m][b b]))))

    ; we put intersect-no-points in this class so all subclasses inherit it:
    ; the intersection of a value with a no-points% is a no-points%
    (define/public (intersect-no-points np) np)

    ; we put intersect-line-segment in this class so all subclasses can inhert it:
    ; the intersection of this with a line-segment% is computed by
    ; first intersecting with the line containing the segment and then
    ; calling the result's intersect-with-segment-as-line-result with the segment
    (define/public (intersect-line-segment seg)
      (send (send this intersect (two-points-to-line (send seg get-x1)
                                                     (send seg get-y1)
                                                     (send seg get-x2)
                                                     (send seg get-y2)))
            intersect-with-segment-as-line-result seg))))

; do *not* change the no-points% definition *except* to add methods if needed
; to implement eval-prog and preprocess-prog.
; everything else is done for you.
; although this is the easiest class, it shows what methods every subclass
; of geometry values needs
(define no-points%
  (class geometry-value%
    (super-new)
    
    (define/public (shift dx dy) this) ; shift a no-points% is a no-points%
    (define/public (intersect other)
      (send other intersect-no-points this)) ; will be a no-points% but follow double-dispatch pattern
    (define/public (intersect-point p) this) ; intersection of a point% and a no-points% is a no-points%
    (define/public (intersect-line line) this) ; intersection of a line% and a no-points% is a no-points%
    (define/public (intersect-vertical-line vline) this) ; intersection of a vertical-line% and a no-points% is a no-points%

    ; if this is the intersection of some shape s and the line containing seg, then we
    ; return the intersection of the shape s and the seg, where seg is an instance of
    ; line-segment%. as a no-points%, that means no intersection between s and the line
    ; containing seg, so must be no intersection between s and seg
    (define/public (intersect-with-segment-as-line-result seg) this)))

; *add* methods to point% class -- do *not* change given code and do not override any methods
(define point%
  (class geometry-value%
    (super-new)
    (init x y)
    (define _x x)
    (define _y y)
    (define/public (get-x) _x)
    (define/public (get-y) _y)

    ))

; *add* methods to line% class -- do *not* change given code and do not override any methods
(define line%
  (class geometry-value%
    (super-new)
    (init m b)
    (define _m m)
    (define _b b)
    (define/public (get-m) _m)
    (define/public (get-b) _b)

    ))

; *add* methods to vertical-line% class -- do *not* change given code and do not override any methods
(define vertical-line%
  (class geometry-value%
    (super-new)
    (init x)
    (define _x x)
    (define/public (get-x) _x)

    ))

; *add* methods to line-segment% class -- do *not* change given code and do not override any methods
; Note: This is the most difficult class, particularly:
;         * preprocess-prog, although the sample solution is only several lines
;         * intersect-with-segment-as-line-result, about 35 lines in the sample solution
; Remember:
;   * (x1,y1) is to the /right/ of (x2,y2) or /above/ if x1 and x2 are float-close to each other
;   * The ML code has all the algorithms you need, you just need to port to Racket
(define line-segment%
  (class geometry-value%
    (super-new)
    (init x1 y1 x2 y2)
    (define _x1 x1)
    (define _y1 y1)
    (define _x2 x2)
    (define _y2 y2)
    (define/public (get-x1) _x1)
    (define/public (get-y1) _y1)
    (define/public (get-x2) _x2)
    (define/public (get-y2) _y2)

    ))

; Note: there is no need for getter methods for the non-value classes

; For each of the non-value classes, do not change the provided code but do add the
; small number of methods needed

(define intersect%
  (class geometry-expression%
    (super-new)
    (init e1 e2)
    (define _e1 e1)
    (define _e2 e2)

    ))

(define let%
  (class geometry-expression%
    (super-new)
    (init s e1 e2)
    (define _s s)
    (define _e1 e1)
    (define _e2 e2)

    ))

(define var%
  (class geometry-expression%
    (super-new)
    (init s)
    (define _s s)
    (define/public (eval-prog env)
      (let ([pr (assoc _s env)])
        (if pr (cdr pr) (error "var not found" _s))))

    ))

(define shift%
  (class geometry-expression%
    (super-new)
    (init dx dy e)
    (define _dx dx)
    (define _dy dy)
    (define _e e)

    ))
