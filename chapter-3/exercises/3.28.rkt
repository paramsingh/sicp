#lang sicp

;;; Exercise 3.28: Define an or-gate as a primitive function
;;; box. Your or-gate constructor should be similar to and-gate.


(define (get-signal wire) '())
(define (after-delay delay action) action)
(define (or-gate-delay) 2)
(define (set-signal! wire new-value) '())
(define (add-action! wire action) '())

(define (logical-or x y)
  (if (or x y) 1 0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((o (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output o)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)