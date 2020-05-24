#lang racket

(require rackunit "press-lang.rkt")

(define fact-def
  '(def fact
     (lambda (n)
       (if (eq? 0 n)
           1
           (* n (fact (- n 1)))))))

(define (real-fact n)
  (if (eq? 0 n)
      1
        (* n (real-fact (- n 1)))))

(define (check-fact n)
  (check-equal? (my-eval-go basic-env fact-def `(fact ,n))
                (real-fact n)
                (string-append "Test factorial for: " (number->string n))))

(define (check-fact-rng)
  (check-fact (random 0 100)))

(check-fact-rng)
