#lang racket

;; Defining numbers in terms of procedures in accordance to the definitions found within Peano Arithmetic
(define zero 
    (lambda (f)
        (lambda (x) x)))

(define add-1
    (lambda (n)
        (lambda (x)
            (lambda (f) (f ((n f) x))))))

(define one
    (lambda (f)
        (lambda (x) (f x))))

(define two
    (lambda (f)
        (lambda (x) (f (f x)))))

(define (peano-add m n)
    (lambda (x)
        (lambda (f)
            ((m f) (f ((n f) x))))))

(define (construct-number n)
  (lambda (x)
    (lambda (f)
      ((n f) (f ((one f) x))))))

