
#lang sicp
;;; Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, if
;;; an account is accessed more than seven consecutive times
;;; with an incorrect password, it invokes the procedure call-the-cops.


(define (call-the-cops)
  (display "cops have been called")
  (newline))

(define (make-account balance password)
  (let ((incorrect-password-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (cond  ((not (eq? p password))
              (begin
                (set! incorrect-password-count (+ incorrect-password-count 1))
                (if (>= incorrect-password-count 7) (call-the-cops))
                (lambda (_) "Incorrect password")))
             ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "Unknown request: MAKE-ACCOUNT"
                          m))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)
((acc 'other-password 'deposit) 50)