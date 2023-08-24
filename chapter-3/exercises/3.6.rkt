#lang sicp


;;; Exercise 3.6: It is useful to be able to reset a random-number
;;; generator to produce a sequence starting from a given value.
;;; Design a new rand procedure that is called with an argument that is either the
;;; symbol generate or the symbol reset and behaves as follows:
;;; (rand 'generate) produces a new random number;
;;; ((rand 'reset) ⟨new-value ⟩) resets the internal state variable to the designated ⟨new-value ⟩.
;;; Thus, by resetting the state, one can generate repeatable sequences.
;;; These are very handy to have when testing and
;;; debugging programs that use random numbers.


(define (rand-update x) (+ x 1))

(define random-init 42)

(define (make-rand)
  (let ((x random-init))
    (lambda (sym)
      (cond ((eq? sym 'generate) (set! x (rand-update x))
                                 x)
            ((eq? sym 'reset) (lambda (reset-val)
                                (set! x reset-val) x))
            (else (error "something went wrong"))))))

(define rand (make-rand))

(rand 'generate)
((rand 'reset) 78)
(rand 'generate)
((rand 'reset) 90)
(rand 'generate)
(rand 'generate)

;;; 43
;;; 78
;;; 79
;;; 90
;;; 91
;;; 92