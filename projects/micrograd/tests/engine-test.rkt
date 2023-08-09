#lang racket

(require "../engine.rkt")

;;; Testing
(define a (make-value 2 (set) 'a))
(define b (make-value 3 (set) 'b))

(define c (add-value a b))
(set-value-label! c 'c)

(backward c)

(print-value a)
(print-value b)
(print-value c)

(display "===================================")
(newline)

(set-value-grad! a 0.0)
(set-value-grad! b 0.0)
(define d (mul-value a b))
(set-value-label! d 'd)
(backward d)

(print-value a)
(print-value b)
(print-value d)

(display "================================")
(newline)
(define x (make-value 2 (set) 'x))
(define y (pow-value x 3))
(set-value-label! y 'y)
(backward y)
(print-value x)
(print-value y)