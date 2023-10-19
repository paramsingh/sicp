;;; Louis is upset that his plan didn’t work. He is willing to go
;;; to any lengths to make his evaluator recognize procedure applications
;;; before it checks for most
;;; other kinds of expressions. Help him by changing the
;;; syntax of the evaluated language so that procedure
;;; applications start with call. For example, instead of
;;; (factorial 3) we will now have to write (call factorial 3)
;;; and instead of (+ 1 2) we will have to write (call + 1 2).

#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (application? exp) (tagged-list? exp 'call))
(define (application-procedure exp)
  (cadr exp))
(define (application-args exp)
  (cddr exp))