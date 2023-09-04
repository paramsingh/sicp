#lang sicp

;;; Exercise 3.29: Another way to construct an or-gate is as
;;; a compound digital logic device, built from and-gates and
;;; inverters. Define a procedure or-gate that accomplishes
;;; this. What is the delay time of the or-gate in terms of
;;; and-gate-delay and inverter-delay?

;;; a or b = not (not a and not b)


(define (make-wire) '())

(define (or-gate a b o)
  (let ((nota (make-wire))
        (notb (make-wire))
        (andwire (make-wire)))
    (inverter a nota)
    (inverter b notb)
    (and-gate nota notb andwire)
    (inverter andwire o)))


;;; delay = i + i + a + i = 3i + a