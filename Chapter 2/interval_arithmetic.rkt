#lang racket

;; Calculates the resistance of two combined resistors
(define parallel-equivalent-resistance
    (lambda (resistor1 resistor2)
        (/ 1
           (+ (/ 1 resistor1)
              (/ 1 resistor2)))))

;; Resistors come with a tolerance of a certain amount, so the combined resistance of two resistors is influenced by the range
;; of tolerance for both of those resistors

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; Assumes the minimum value of the sum is the sum of the lower-bound values, likewise for the maximum value
(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

;; We are multiplying interval x by the reciprocal of interval y
(define (div-interval x y)
    (cond ((and (zero? (lower-bound y)) (zero? (upper-bound y))) (raise "Cannot divide by an interval of size zero"))
          (else (mul-interval x
                              (make-interval (/ 1.0 (upper-bound y))
                                             (/ 1.0 (lower-bound y)))))))

(define (sub-interval x y)
    (let ((smallest-lb (min (lower-bound x) (lower-bound y)))
          (largest-lb (max (lower-bound x) (lower-bound y)))
          (smallest-hb (min (upper-bound x) (upper-bound y)))
          (largest-hb (max (upper-bound x) (upper-bound y))))
        (make-interval (- largest-lb smallest-lb)
                       (- largest-hb smallest-hb))))
