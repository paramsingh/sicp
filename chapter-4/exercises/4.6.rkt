#lang sicp

;;; Exercise 4.6: let expressions are derived expressions, because
;;;
;;; (let ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
;;;   ⟨body⟩)
;;;
;;; is equivalent to
;;;
;;; ((lambda (⟨var1⟩ ... ⟨varn⟩)
;;;    ⟨body⟩)
;;;  ⟨exp1⟩
;;;   ...
;;;  ⟨expn⟩)
;;;
;;; Implement a syntactic transformation let->combination
;;; that reduces evaluating let expressions to evaluating combinations
;;; of the type shown above, and add the appropriate
;;; clause to eval to handle let expressions.

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

(let->combination '(let ((x 1) (y 2)) (+ x y)))