#lang racket
(require scheme/runtime-config)

;; Exercise 3
(define sum-of-squares-of-two-largest 
    (lambda (x y z)
        (if (>= x y)
            (sum-of-squares x (if (>= y z)
                                  y
                                  z))
            (sum-of-squares y z))))

;; Exercise 11
;; Recursive Process
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (double (f (- n 2)))
           (treble (f (- n 3))))))

;; Iterative Process
(define (f* n)
    (define (f-iter a b c count)
        (if (= count 0)
            a
            (f-iter (+ a b c)
                    (double a)
                    (* (/ 3 2) b)
                    (dec count))))
    (if (< n 3)
        n
        (f-iter 2 1 0 n)))

;; Exercise 12
(define (pascal-triangle n k)
  (define (binomial-coefficient n k)
    (cond ((or (= k 0) (= k n)) 1)
          (else (+ (binomial-coefficient (- n 1)
                                         (- k 1))
                   (binomial-coefficient (- n 1)
                                         k)))))
  (if (and (<= k n)
           (>= n 0)
           (>= k 0))
      (binomial-coefficient n k)
      (error "No corresponding value in Pascals Triangle")))

;; Exercise 16
(define (exp b n)
  (define (exp-iter-log base counter a)
    (cond ((= counter 0) a)
          ((even? counter) (exp-iter-log (square base)
                                         (/ counter 2)
                                         a))
          (else (exp-iter-log base
                              (dec counter)
                              (* base a)))))
  (exp-iter-log b n 1))

;; Exercise 17
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-* (double a) (half-of b)))
        (else (+ a (fast-* a (- b 1))))))

;; Exercise 18
(define (mult a b)
  (define (mult-iter-log integer counter result)
    (cond ((= counter 0) result)
          ((even? counter) (mult-iter-log (double integer)
                                          (half-of counter)
                                          result))
          (else (mult-iter-log integer)
                (dec counter)
                (+ integer result))))
  (mult-iter-log a b 0))

;; Exercise 22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (define (generate-list-of-successive-odd-numbers)
    (cond ((> low high) '())
          ((even? low) (generate-list-of-successive-odd-numbers (inc low) high))
          (else (cons low (generate-list-of-successive-odd-numbers (dinc low) high)))))
  (define (dinc n)
    (inc (inc n)))
  (map timed-prime-test (generate-list-of-successive-odd-number low high)))      

;; Useful functions
(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (square x) (* x x))

(define (double x)
    (* 2 x))

(define (half-of x)
    (/ x 2))

(define (treble x)
    (* 3 x))

(define (dec x)
    (- x 1))

;; Exercise 30


