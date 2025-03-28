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

(define (caro p a)
  (fresh (d)
    (== (cons a d) p)))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(run-tests
 (test-suite "examples"
  (test-equal? "run* appendo x y '(1 2 3 4 5)"
    (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
      '((() (1 2 3 4 5))
        ((1) (2 3 4 5))
        ((1 2) (3 4 5))
        ((1 2 3) (4 5))
        ((1 2 3 4) (5))
        ((1 2 3 4 5) ()))
  )
  (test-equal? "run* appendo x '() '(1 2 3)"
    (run* (x) (appendo x '() '(1 2 3)))
      '((1 2 3))
  )
  (test-equal? "run* appendo '() y '(4 5 6)"
    (run* (y) (appendo '() y '(4 5 6)))
      '((4 5 6))
  )
  (test-equal? "run* appendo '(1 2) '(3 4) z"
    (run* (z) (appendo '(1 2) '(3 4) z))
      '((1 2 3 4))
  )
  (test-equal? "run 2 appendo x y '(1 2 3 4 5)"
    (run 2 (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
      '((() (1 2 3 4 5))
        ((1) (2 3 4 5)))
  )
  (test-equal? "run 2 appendo x '() '(1 2 3)"
    (run 2 (x) (appendo x '() '(1 2 3)))
      '((1 2 3))
  )
  (test-equal? "run 2 appendo '() y '(4 5 6)"
    (run 2 (y) (appendo '() y '(4 5 6)))
      '((4 5 6))
  )
  (test-equal? "run 2 appendo '(1 2) '(3 4) z"
    (run 2 (z) (appendo '(1 2) '(3 4) z))
      '((1 2 3 4))
  )
  (test-equal? "run 0 appendo '(1 2) '(3 4) z"
    (run 0 (z) (appendo '(1 2) '(3 4) z))
      '()
  )
  (test-equal? "caro '(1 2 3) 8"
    (run* (q) (caro '(1 2 3) 8))
      '()
  )
  (test-equal? "caro '(1 2 3) 1"
    (run* (q) (caro '(1 2 3) 1))
      '(_.0)
  )
  (test-equal? "caro '(1 2 3) a"
    (run* (a) (caro '(1 2 3) a))
      '(1)
  )
  (test-equal? "caro p 1"
    (run* (p) (caro p 1))
      '((1 . _.0))
  )
 ))