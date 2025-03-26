#lang racket

(require "microkanren.rkt")
(require "extension.rkt")

;;; taken from https://github.com/jasonhemann/microKanren/blob/0f4505db0d2525fc3d567c5183e45f28992cfe72/miniKanren-wrappers.scm#L106-L121

(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(run-tests
 (test-suite "examples"
  (test-equal? "appendo x y '(1 2 3 4 5)"
    (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
      '((() (1 2 3 4 5))
        ((1) (2 3 4 5))
        ((1 2) (3 4 5))
        ((1 2 3) (4 5))
        ((1 2 3 4) (5))
        ((1 2 3 4 5) ())))
 ))