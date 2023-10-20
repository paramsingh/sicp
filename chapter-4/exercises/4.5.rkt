#lang sicp

;;; Exercise 4.5: Scheme allows an additional syntax for cond
;;; clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test⟩ evaluates to a
;;; true value, then ⟨recipient⟩ is evaluated. Its value must be a
;;; procedure of one argument; this procedure is then invoked
;;; on the value of the ⟨test⟩, and the result is returned as the
;;; value of the cond expression. For example
;;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;;       (else false))
;;; returns 2. Modify the handling of cond so that it supports
;;; this extended syntax.


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (predicate-clause clause) (car clause))
(define (actions-clause clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-else-clause? clause) (eq? predicate-clause 'else))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (if (cond-else-clause? (car clauses))
          (if (null? (cdr clauses))
              (sequence->exp (actions-clause (car clauses)))
              (error "else is not last"))
          (if (eq? (cadr (car clauses) '=>))
              (make-if
               (predicate-clause (car clauses))
               ;;; we make a list to signify application of function
               (list (cadr (actions-clause (car))) (predicate-clause (car clauses)))
               (expand-clauses (cdr clauses)))
              (make-if
               (predicate-clause (car clauses))
               (sequence->exp (actions-clause (car clauses)))
               (expand-clauses (cdr clauses)))))))