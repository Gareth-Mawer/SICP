#lang racket

;; Exercise 7
;; This program is better suited to calculating the square root of large numbers, not small ones. 
;; The precision of the test is too little for small numbers but not sufficiently precise to make
;; calculating the square root of large numbers sufficiently long. 
(define (sqrt x)
    (define (improve guess)
        (average guess (/ x guess)))
    (define (good-enough? guess)
        (< (abs (- (improve guess) guess)) 0.0001))
    (define (sqrt-iter guess x)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess)
            x)))
    (sqrt-iter 1.0 x))

;; Exercise 8
;; Note the use of lexical scoping
(define (cubicroot x)
    (define (improve guess)
        (/ (+ (/ x (square guess))
              (double guess))
            3)
    (define (good-enough? guess)
        (< (abs (- (improve guess) guess)) 0.0001))
    (define (cubic-iter guess)
        (if (good-enough? guess)
            guess
            (cubic-iter (improve guess)
            x)))
    (cubic-iter 1.0)))