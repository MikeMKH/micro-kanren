#lang racket

(require (except-in rackunit fail))
(require rackunit/text-ui)

(run-tests
 (test-suite "example from paper"
  (test-equal? "hello world" (cdr `(1 . a)) 'a)
 ))