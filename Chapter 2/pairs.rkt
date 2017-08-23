#lang racket

;; This procedure is the implementation of cons in Lisp
;; We build data objects in lisp through pairs, which itself is data made purely out of procedures
(define (cons a b)
    (lambda (pick)
        (cond ((= pick 1) a)
              ((= pick 2) b))))

(define (car x) (x 1))
(define (cdr x) (x 2))

;; Alternative implementation of cons, car and cdr
(define (cons a b)
    (lambda (pick)
        (pick x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

;; Implementation of the map procedure. This is a recursive process
(define (map procedure l)
    (if (null? l)
        nil
        (cons (procedure (car l))
              (map procedure (cdr l)))))

;; Implementation of the for-each procedure. This procedure doesn't construct a new list like map does.
(define (for-each procedure l)
    (cond ((null? l) (quote()))
          ((else (procedure (car l))
                 (for-each procedure (cdr l))))))