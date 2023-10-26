#lang sicp

;;; Exercise 4.22: Extend the evaluator in this section to support the special form let.
;;; (See Exercise 4.6.)

(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (let-param-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (let->combination exp)
  (define (parameter-names param-pairs)
    (cond ((null? param-pairs) '())
          (else (cons (caar param-pairs) (parameter-names (cdr param-pairs))))))
  (define (parameter-values param-pairs)
    (cond ((null? param-pairs) '())
          (else (cons (cadar param-pairs) (parameter-values (cdr param-pairs))))))
  (cons
   (make-lambda
    (parameter-names (let-param-pairs exp))
    (sequence->exp (let-body exp)))
   (parameter-values (let-param-pairs exp))))

;;; (define (analyze-let exp)
;;;   (let ((combproc (analyze (let->combination exp))))
;;;     combproc))
(let->combination '(let ((x 1) (y 2)) (+ x y)))