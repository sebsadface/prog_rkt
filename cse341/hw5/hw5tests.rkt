#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

  (let ((test-env (list (cons "x" (int 5)) (cons "y" (int 2)))))
    (check-equal? (eval-under-env (var "x") test-env) (int 5))  ;; var?

    (check-equal? (eval-under-env (add (var "x") (var "y")) test-env) (int 7)) ;; add?

    (check-equal? (eval-under-env (int 7) test-env) (int 7)) ;; int?

    (check-equal? (eval-under-env (isgreater (var "x") (var "y")) test-env) (int 1)) ;; isgreater?

    (check-equal? (eval-under-env (ifnz (var "x") (int 1) (int 0)) test-env) (int 1)) ;; ifnz?

    (check-equal? (eval-under-env (fun (var "f") (var "a") (add (var "a") (var "x"))) test-env) (closure test-env (fun (var "f") (var "a") (add (var "a") (var "x"))))) ;; fun?

    (check-equal? (eval-under-env (mlet "z" (add (var "x") (var "y")) (add (var "z") (var "x"))) test-env) (int 12)) ;; mlet?

    (check-equal? (eval-under-env (call (fun "f" "a" (add (var "a") (int 2))) (int 3)) test-env) (int 5)) ;; call?

    (check-equal? (eval-under-env (apair (var "x") (var "y")) test-env) (apair (int 5) (int 2))) ;; apair?

    (check-equal? (eval-under-env (first (apair (var "x") (var "y"))) test-env) (int 5)) ;; first?

    (check-equal? (eval-under-env (second (apair (var "x") (var "y"))) test-env) (int 2)) ;; second?

    (check-equal? (eval-under-env (ismunit (munit)) test-env) (int 1)) ;; ismunit?

    (check-equal? (eval-under-env (munit) test-env) (munit)) ;; munit?

    (check-exn exn:fail? (lambda () (eval-under-env (var "z") test-env))) ;; error case
  )

   (let ((test-env (list (cons "x" (int 5)) (cons "y" (int 2)) (cons "z" (int 3)))))

   ; Test for ifmunit
   (check-equal? (eval-under-env (ifmunit (munit) (var "x") (var "y")) test-env)
                 (int 5)
                 "ifmunit test 1")

   (check-equal? (eval-under-env (ifmunit (var "x") (var "y") (var "z")) test-env)
                 (int 3)
                 "ifmunit test 2")

   ; Test for mlet*
   (check-equal? (eval-under-env (mlet* (list (cons "a" (int 10)) (cons "b" (var "x"))) (add (var "a") (var "b"))) test-env)
                 (int 15)
                 "mlet* test 1")

   (check-equal? (eval-under-env (mlet* (list (cons "a" (add (var "x") (var "y"))) (cons "b" (add (var "a") (var "z")))) (var "b")) test-env)
                 (int 10)
                 "mlet* test 2")

   ; Test for ifeq
   (check-equal? (eval-under-env (ifeq (var "x") (var "y") (var "z") (int 0)) test-env)
                 (int 0)
                 "ifeq test 1")

   (check-equal? (eval-under-env (ifeq (var "y") (var "y") (var "z") (int 0)) test-env)
                 (int 3)
                 "ifeq test 2")

   (check-equal? (eval-under-env (ifeq (var "y") (var "x") (var "z") (int 0)) test-env)
                 (int 0)
                 "ifeq test 3")

  (check-equal?
   (mupllist->racketlist
    (eval-under-env
      (call (call mupl-all-gt (int 3))
            (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5))))
      test-env))
  (list (int 4) (int 5))
  "mupl-all-gt test")

  (check-equal?
   (mupllist->racketlist
    (eval-under-env
      (call (call mupl-all-gt (int 3))
            (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5))))
      test-env))
  (list (int 4) (int 5))
  "mupl-all-gt test")

     )

   
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)