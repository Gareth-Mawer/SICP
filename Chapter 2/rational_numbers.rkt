#lang racket

;; Construct a rational number
(define (make-rat numerator denominator)
  (let ((g (gcd numerator denominator)))
    (cond ((or (and (> numerator 0) (> denominator 0)) 
               (and (< numerator 0) (< denominator 0))) (cons (/ numerator g) (/ denominator g)
          (else (cons (- (/ numerator g))
                      (/ denominator g))))))

;; Uses Euclid's Algorithm to calculate the Greatest Common Denominator
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

;; Get elements from a rational number
(define (numerator x) (car x))
(define (denominator x) (cdr x))

;; Procedures associated with rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))

(define (subtract-rat x y)
  (make-rat (- (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y)))))

(define (multiply-rat x y)
  (make-rat (* (numerator x) (numerator y))
            (* (denominator x) (denominator y))))

(define (divide-rat x y)
  (make-rat (* (numerator x) (denominator y))
            (* (denominator x) (numerator y))))

(define (equal-rat? x y)
  (= (* (numerator x) (denominator y))
     (* (denominator x) (numerator y))))

(define (print-rat rat)
  (newline)
  (display (numerator rat))
  (display "/")
  (display (denominator rat)))

(define A (make-rat -1 2))
(define B (make-rat -1 -4))
(add-rat A B)