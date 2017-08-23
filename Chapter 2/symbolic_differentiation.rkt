#lang racket

;; Computing the derivative of a function, it will lead to noisy answers
(define deriv-func
    (let ((dx 0.00001))
      (lambda (f)
        (lambda (x)
            (/ (-  (f (+ x dx))
                   (f x))
                dx)))))

;; This procedure is not calculating the derivative of a function, it is manipulating the
;; formula of the function into the formula of the derivative of that function. 
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) car)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-exponent (make-minus (exponent exp) '1))))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? num exp)
  (and (number? exp)
       (= exp num)))

(define (make-sum . additives)
  (cond ((andmap number? additives) (+ additives))
        (else (list '+ additives))))

(define (make-product . products)
  (cond ((ormap (curry =number? 0) products) 0)
        ((andmap number? products) (* products))
        (else (list '* products))))

(define (make-minus m1 m2)
  (cond ((=number? 0 m1) (- m2))
        ((=number? 0 m2) m1)
        ((and (number? m1) (number? m2)) (- m1 m2))
        (else (list '- m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? 0 base) 0)
        ((=number? 0 exponent) 1)
        ((=number? 1 exponent) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list '** base exponent))))

(define (make-exponent exp)
  (cond ((number? exp) exp)
        (else (list (car exp) (cadr exp) (caddr exp)))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(deriv '(** x y) 'x)