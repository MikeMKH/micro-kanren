#lang racket

; code from http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assq (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v)
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s)))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

; test examples
(require (except-in rackunit fail))
(require rackunit/text-ui)

; 2
(define empty-state '(() . 0))
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

; 5.1
(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

; 5.2
(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
      (let (($ (pull $)))
        (cond
          ((null? $) '())
          (else (cons (car $) (take (- n 1) (cdr $))))))))

; 5.3
(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let ((n (reify-name (length s))))
         (cons `(,v . ,n) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s)))
      (else v))))

; 5.4
(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...)))))))

(run-tests
 (test-suite "paper examples"
  (test-equal? "== q 5" ((call/fresh (lambda (q) (== q 5))) empty-state) '((((#(0) . 5)) . 1)))
  (test-equal? "a-and-b" (a-and-b empty-state)
    '((((#(1) . 5) (#(0) . 7)) . 2)
      (((#(1) . 6) (#(0) . 7)) . 2)))
  (test-equal? "five" ((call/fresh five) empty-state) '((((#(0) . 5)) . 1)))
  (test-equal? "five-and-six" (five-and-six empty-state) '((((#(0) . 5)) . 1) (((#(0) . 6)) . 1)))
 ))