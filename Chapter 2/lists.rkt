#lang racket
(define (square n) (* n n))
(define (dec n) (- n 1))

;; length: list -> num. Returns the length of a list
(define (length l)
    (if (null? l)
        0
        (+ 1 (length (cdr l)))))

;; last-pair: list -> list. Returns the last element of a list
(define (last-pair l)
    (if (= (length l) 1)
        l
        (last-pair (cdr l))))

(define (reverse l)
    (let ((last-list-index (dec (length l))))
      ()))

(define (same-parity x . w)
    (define (same-parity? a b)
        (cond ((and (even? a) (even? b)) #t)
              ((and (odd? a) (odd? b)) #t)
              (else #f)))
    (define parity-list (list x))
    (cond ((null? (car w)) (quote()))
            ((same-parity? (car w) x) (append parity-list (car w))
                                      (same-parity x . (cdr w)))
            (else (same-parity x . (cdr w)))))

;(define (scale-list items)
;    (map (lambda (x) (* x factor))
;         items))

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items)) 
              (square-list (cdr items)))))

(define (square-list-iter items)
    (map square items))

(define (count-leaves l)
    (cond ((null? l) 0)
          ((not (pair? l)) 1)
          (else (+ (count-leaves (car l)) 
                   (count-leaves (cdr l))))))

