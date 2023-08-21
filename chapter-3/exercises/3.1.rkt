#lang sicp

;;; Exercise 3.1: An accumulator is a procedure that is called
;;; repeatedly with a single numeric argument and accumulates its arguments into a sum.
;;; Each time it is called, it
;;; returns the currently accumulated sum. Write a procedure
;;; make-accumulator that generates accumulators, each maintaining an
;;; independent sum.e input to make-accumulator
;;; should specify the initial value of the sum

(define (make-accumulator initial-val)
  (let ((current-val initial-val))
    (lambda (x)
      (begin
        (set! current-val (+ current-val x))
        current-val))))


(define A (make-accumulator 5))
(A 10)
(A 10)
(define B (make-accumulator 0))
(B 10)
(A 100)

;;; param@Params-MacBook-Pro exercises % racket 3.1.rkt
;;; 15
;;; 25
;;; 10
;;; 125