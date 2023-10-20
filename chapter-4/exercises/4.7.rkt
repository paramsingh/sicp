#lang sicp


;;; Exercise 4.7: let* is similar to let, except that the bindings of the let* variables
;;; are performed sequentially from
;;; left to right, and each binding is made in an environment in
;;; which all of the preceding bindings are visible. For example
;;;
;;; (let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
;;;   (* x z))
;;;
;;; returns 39.
;;; Explain how a let* expression can be rewritten
;;; as a set of nested let expressions, and write a procedure
;;; let*->nested-lets that performs this transformation. If
;;; we have already implemented let (Exercise 4.6) and we
;;; want to extend the evaluator to handle let*, is it sufficient
;;; to add a clause to eval whose action is
;;;
;;; (eval (let*->nested-lets exp) env)
;;;
;;; or must we explicitly expand let* in terms of non-derived
;;; expressions?


;;; (let ((x 3))
;;;   (let ((y (+ x 2)))
;;;     (let ((z (+ x y 5)))
;;;       (* x z))))

(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-let pairs body)
  (list 'let pairs body))

(define (let*-body exp) (cddr exp))
(define (let*-bindings exp) (cadr exp))

(define (let*->nested-lets exp)
  (define (convert bindings)
    (cond ((null? bindings) (sequence->exp (let*-body exp)))
          (else (make-let (list (car bindings)) (convert (cdr bindings))))))
  (convert (let*-bindings exp)))


(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (print "hello") (+ x y)))