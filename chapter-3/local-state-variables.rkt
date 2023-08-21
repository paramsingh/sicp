#lang sicp

;;; (define balance 100)

;;; (define (withdraw x)
;;;   (if (>= balance x)
;;;       (begin (set! balance (- balance x))
;;;              balance)
;;;       "Insufficient funds"
;;;       ))

(define new-withdraw
  (let ((balance 100))
    (lambda (x)
      (if (>= balance x)
          (begin (set! balance (- balance x))
                 balance)
          "Insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)