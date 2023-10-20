#lang sicp

;;;  and: The expressions are evaluated from left to right.
;;; If any expression evaluates to false, false is returned;
;;; any remaining expressions are not evaluated. If all the
;;; expressions evaluate to true values, the value of the
;;; last expression is returned. If there are no expressions
;;; then true is returned.

(define (op-and exp env)
  (define (eval-and exps)
    (cond ((null? exps) true)
          ((eval (car exps) env) (eval-and (cdr exps)))
          (else false)))
  (eval-and (cdr exp)))

;;; or: The expressions are evaluated from left to right.
;;; If any expression evaluates to a true value, that value
;;; is returned; any remaining expressions are not evaluated.
;;; If all expressions evaluate to false, or if there are
;;; no expressions, then false is returned.
(define (op-or exp env)
  (define (eval-or exps)
    (cond ((null? exps) false)
          ((eval (car exps) env) true)
          (else (eval-or (cdr exps)))))
  (eval-or (cdr exp)))