#lang sicp

;;; Exercise 3.7: Consider the bank account objects created by
;;; make-account, with the password modification described
;;; in Exercise 3.3. Suppose that our banking system requires
;;; the ability to make joint accounts.
;;; Define a procedure make-joint that accomplishes this.
;;; make-joint should take three
;;; arguments. The first is a password-protected account. The
;;; second argument must match the password with which the
;;; account was defined in order for the make-joint operation
;;; to proceed. The third argument is a new password.
;;; make-joint is to create an additional access to the original
;;; account using the new password. For example, if peter-acc
;;; is a bank account with password open-sesame, then
;;;
;;; (define paul-acc
;;;   (make-joint peter-acc 'open-sesame 'rosebud))
;;;
;;; will allow one to make transactions on peter-acc using the
;;; name paul-acc and the password rosebud. You may wish
;;; to modify your solution to Exercise 3.3 to accommodate this
;;; new feature.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p op)
    (cond
      ((eq? op 'check-password) (eq? p password))
      ((not (eq? p password)) (lambda (_) "Incorrect password"))
      ((eq? op 'withdraw) withdraw)
      ((eq? op 'deposit) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT"
                   op))))
  dispatch)

(define (make-joint old-acc password new-password)
  (define (joint-dispatch p op)
    (cond
      ((not (eq? p new-password)) (lambda (_) "incorrect password for joint account"))
      (else (old-acc password op))))
  (cond
    ((not (old-acc password 'check-password)) (error "wrong initial password"))
    (else joint-dispatch)))

(define acc (make-account 100 'secret-password))

;;; if we give incorrect password while creating joint account, it breaks
;;; (define joint-acc (make-joint acc 'wrong-password 'blah)) ;;; wrong initial password

(define joint-acc (make-joint acc 'secret-password 'joint-password))

((joint-acc 'joint-password 'deposit) 50)
((joint-acc 'joint-password 'withdraw) 20)
((acc 'secret-password 'deposit) 10)

((joint-acc 'incorrect 'withdraw) 20)

;;; 150
;;; 130
;;; 140
;;; "incorrect password for joint account"
