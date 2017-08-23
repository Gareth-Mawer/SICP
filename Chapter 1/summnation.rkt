#lang racket

(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (square n) (* n n))
(define (cube n) (* n n n))

(define (sum term a next b)
  (cond ((> a b) 0)
        (else (+ (term a)
                 (sum term (next a) next b)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd a (remainder a b))))

(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;; Exercise 29
(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (coefficient k)
    (cond ((= k 0) 1)
          ((even? k) 2)
          (else 4)))
  (* (/ h 3)
     (sum (λ (k) (* (coefficient k)
                    (y k)))
          0
          inc
          n)))

;; Exercise 30
(define (iterative-sum term a next b)
  (define (sum-iter a result)
    (if (= a b)
        result
        (sum-iter (next a) (+ (term a) result))))
  (sum-iter a b))

;; Exercise 31a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (iterative-product identity 1 inc n))

(define (pi-approx n)
  (define (square x) (* x x))
  (* 4
     (product (λ (x)
                (cond ((odd? x) (/ (+ x 1)
                                   (+ x 2)))
                      ((even? x) (/ (+ x 2)
                                    (inc x)))))
              1.0
              inc
              n)))

;; Exercise 31b
(define (iterative-product term a next b)
  (define (product-iter a result)
    (if (= a b)
        result
        (product-iter (next a) (* (term a) result))))
  (product-iter a b))

;; Exercise 32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

;; Exercise 32b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (= a b)
        (term result)
        (iter (next a) (combiner (term a) result))))
  (iter a b))

;; Exercise 33
(define (filtered-accumulate combiner null-value filter term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate combiner null-value filter term (next a) next b)))
        (else (filtered-accumulate combiner null-value filter term (next a) next b))))

; Find the sum of the squares of prime numbers given an interval
(define (sum-of-primes-squared a b)
  (filtered-accumulate +
                       0
                       prime?
                       square
                       a
                       inc
                       b))

; Find the product of all positive integers less than n that are relatively prime to n
; i.e. for all positive integers i < n, such that GCD(i, n) = 1
(define (product-of-coprimes b)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (relatively-prime? i)
    (= (gcd i b) 1))
  (filtered-accumulate +
                     0
                     relatively-prime?
                     (λ (x) x)
                     1
                     inc
                     10))

;(product-of-coprimes 40)

;; Exercise 36
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display "***")
      (newline)
      (display next)
      (newline)
      (cond ((close-enough? guess next) next)
            (else (try next)))))
  (try first-guess))

(fixed-point (λ (x) (/ (log 1000)
                       (log x)))
             2)

;; Exercise 37
(define (cont-frac n d k)
  (define (cont-frac-recur i)
     (if (> i k)
         1
         (/ (n i)
            (+ (d i) (cont-frac-recur (inc i))))))
  (cont-frac-recur 1))

; The iterative version is not my own work. This method relies on calculating the kth numerator and denominator and then divivding
; the (k - 1) th numerator and denominator, while adding the result of the kth calculation to the (k - 1)
; denominator.
(define (cont-frac-iter n d k)
  (define (iteration i result)
    (if (= i 0)
        result
        (iteration (dec i)
                   (/ (n i)
                      (+ (d i) result)))))
  (iteration k 0))

;; When k = 12, phi is correct to four decimal places. 
(define phi (/ 1
               (cont-frac (λ (i) 1.0)
                          (λ (i) 1.0)
                          12)))

;; Exercise 38
(define e-minus-2
  (cont-frac (λ (i) 1)
             (λ (i) (cond ((= (gcd (inc i) 3) 3) i)
                          (else 1)))
             100000000000))

(define e (+ 2.0 e-minus-2))
e

;; Exercise 39
(define (tan-cf x k)
  (cont-frac (λ (i) (if (= i 1)
                        x
                        (- (square x))))
             (λ (i) (- (* 2 i) 1))
             k))

;(tan-cf 1.0 100000)

;; Exercise 40
(define (cubic a b c)
  (λ (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 41
(define (double f)
  (λ (x)
    (f (f x))))

;; Exercise 42
(define (compose f g)
  (λ (x)
    (f (g x))))

;((compose square inc) 6)

;; Exercise 43
(define (repeated f x)
  (λ (y)
    (cond ((= x 1) (f y))
          (else (f ((repeated f (- x 1)) y))))))

((repeated square 2) 5)

;; Exercise 44
(define (smooth f)
  (λ (x)
    (let ((dx 0.0001))
      (/ (+ (- (f x) dx)
            (f x)
            (+ (f x) dx))
         3))))

(define (nth-smooth f n)
  (λ (x)
    (let ((dx 0.0001))
      (((repeated smooth n) f) x))))

;; Exercise 46




