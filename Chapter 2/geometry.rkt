#lang racket

;; Constructs a two-dimensional vector
(define (make-point x y)
    (cons x y))

(define (x-cor point) (car point))
(define (y-cor point) (cdr point))

(define (print-point point)
    (newline)
    (display "(")
    (display (x-cor point))
    (display ",")
    (display (y-cor point))
    (display ")"))

;; Constructs a line segment
(define (make-line-segment point1 point2)
    (cons point1 point2))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;; Procedures associated with line-segments
(define (midpoint segment)
    (let ((a (start-segment segment)
          (b (end-segment segment))))
        (make-vector
            (average (xcor a) (xcor b))
            (average (ycor a) (ycor b)))))

(define (length segment)
    (let ((dx (- (xcor (end-segment segment))
                (xcor (start-segment segment))))
          (dy (- (ycor (end-segment segment))
                (ycor (start-segment segment)))))
        (pythagoras dx dy)))

;; Constructs a rectangle, or square
;; First Implementation, comment out if using different implementation
(define (make-rectangle length width)
    (let ((origin (make-point 0 0))
          (p2 (make-point 0 length))
          (p3 (make-point width length))
          (p4 (make-point width 0)))
        (list (make-line-segment origin p2)
              (make-line-segment p2 p3)
              (make-line-segment p3 p4)
              (make-line-segment p4 orgin))))

(define (rectangle-length rectangle) (y-cor (end-segment (car rectangle))))
(define (rectangle-width rectangle) (x-cor (start-segment (caddr rectangle))))

;; Second Implementation, comment out if using different implementation
(define (make-rectangle length width)
    (let ((origin (make-point 0 0))
          (p2 (make-point 0 length))
          (p3 (make-point width length))
          (p4 (make-point width 0)))
        (cons (cons (make-line-segment origin p2)
                    (make-line-segment p2 p3))
              (cons (make-line-segment p3 p4)
                    (make-line-segment p4 origin)))))

(define (rectangle-length rectangle) (y-cor (end-segment (car (car rectangle)))))
(define (rectangle-width rectangle) (x-cor (end-segment (car (cdr rectangle)))))

;; Procedures associated with a rectangle
(define (area-rectangle rectangle) 
    (* (rectangle-length rectangle) (rectangle-width rectangle)))

(define (perimeter-rectangle rectangle)
    (+ (double (rectangle-length rectangle)) (double (rectangle-width rectangle))))

;; Additional useful functions
(define (pythagoras a b)
    (sqrt (+ (square a) (square b))))

(define (average x y)
    (/ (+ x y) 2))

(define (square x) (* x x))
