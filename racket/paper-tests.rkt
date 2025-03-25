#lang racket

(require "microkanren.rkt")

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 2
(define a-and-b
  (conj
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh (lambda (b) (disj (== b 5) (== b 6))))))

; 4.1
(define (fives x) (disj (== x 5) (fives x)))
(define (five x) (== x 5))

; 4.3
(define (sixes x)
  (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))
(define fives-and-sixes
  (call/fresh (lambda (x) (disj (fives x) (sixes x)))))
(define (six x) (== x 6))
(define five-and-six
  (call/fresh (lambda (x) (disj (five x) (six x)))))

(run-tests
 (test-suite "paper examples"
  (test-equal? "== q 5" ((call/fresh (lambda (q) (== q 5))) empty-state) '((((#(0) . 5)) . 1)))
  (test-equal? "a-and-b" (a-and-b empty-state)
    '((((#(1) . 5) (#(0) . 7)) . 2)
      (((#(1) . 6) (#(0) . 7)) . 2)))
  (test-equal? "five" ((call/fresh five) empty-state) '((((#(0) . 5)) . 1)))
  (test-equal? "five-and-six" (five-and-six empty-state) '((((#(0) . 5)) . 1) (((#(0) . 6)) . 1)))
 ))