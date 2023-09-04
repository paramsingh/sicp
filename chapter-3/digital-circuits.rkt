#lang sicp

(define (make-wire) '())

(define a (make-wire))
(define b (make-wire))
(define d (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (and-gate a b c)
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((x make-wire)
        (y make-wire)
        (z make-wire))
    (half-adder b c-in x z)
    (half-adder a x sum y)
    (or-gate y z c-out)
    'ok))

;;; (define (get-signal <wire>))
;;; (define (set-signal! <wire> new-value))
;;; (define (add-action! <wire> action-procedure)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-input (logical-not (get-signal input))))
      (after-delay inverter-delay ((lambda () (set-signal! output new-input))))))
  (add-action! input invert-input)
  'ok)

(define (logical-and x y)
  (cond ((= x 1) y)
        ((= y 1) x)
        (else 0)))

(define (and-gate input1 input2 output)
  (define (and-gate-function)
    (let ((o (logical-and (get-signal input1) (get-signal input2))))
      (after-delay and-gate-delay ((lambda () (set-signal! output o))))))
  (add-action! input1 and-gate-function)
  (add-action! input2 and-gate-function)
  'ok)