#lang sicp


;;; Exercise 3.48: Explain in detail why the deadlock-avoidance
;;; method described above, (i.e., the accounts are numbered,
;;; and each process aempts to acquire the smaller-numbered
;;; account first) avoids deadlock in the exchange problem.
;;; Rewrite serialized-exchange to incorporate this idea. (You
;;; will also need to modify make-account so that each account
;;; is created with a number, which can be accessed by sending
;;; an appropriate message.)

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true
      (begin (set-car! cell true) false)))


(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex ''acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))


(define (make-serializer)
  (let ((mutex make-mutex))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (create-id-generator)
  (let ((next-id 0))
    (lambda ()
      (set! next-id (+ next-id 1))
      next-id)))

(define generate-account-id (create-id-generator))

(define (make-account-and-serializer balance)
  (define (make-account-with-id x)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'id) x)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch))
  (make-account-with-id (generate-account-id)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((id1 ((account1 'id)))
        (id2 ((account2 'id))))
    (let* ((first-account (if (< id1 id2) account1 account2))
           (second-account (if (< id1 id2) account2 account1))
           (serializer1 (first-account 'serializer))
           (serializer2 (second-account 'serializer)))
      ((serializer2 (serializer1 exchange))
       account1
       account2))))