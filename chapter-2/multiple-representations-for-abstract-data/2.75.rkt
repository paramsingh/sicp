#lang sicp

;;; Exercise 2.75: Implement the constructor make-from-magang in message-passing style.
;;; This procedure should be analogous to the make-from-real-imag procedure given above.
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* mag (cos ang)))
      ((eq? op 'imag-part) (* mag (sin ang)))
      ((eq? op 'magnitude) mag)
      ((eq? op 'angle) ang)))
  dispatch)

(define (apply-generic op arg) (arg op))

(define number (make-from-mag-ang 10 (/ 3.14 2)))
(apply-generic 'real-part number)
(apply-generic 'imag-part number)
(apply-generic 'magnitude number)
(apply-generic 'angle number)