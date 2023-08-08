#lang racket

(require racket/set)

(define (make-value val children op)
  (list
   'val
   val
   children
   0
   op))

(define (data val)
  (if (number? val)
      (make-value val (set) null)
      (cadr val)))

(define (children val)
  (caddr val))

(define (gradient val) (cadddr val)) ;; goddamn this is ugly

(define (operation val) (cadr (cdddr val))) ;; big puke

(define (print-value val)
  (let ((value (data val)))
    (display "value: ")
    (display value)
    (newline)))

(define (add-value val1 val2)
  (make-value (+
               (data val1)
               (data val2)) (set val1 val2) 'add))

(define (mul-value val1 val2)
  (make-value (*
               (data val1)
               (data val2)) (set val1 val2) 'mul))

(define a (make-value 2 (set) null))
(define b (make-value 3 (set) null))

(print-value (mul-value a b))
