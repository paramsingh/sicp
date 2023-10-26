#lang sicp

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-clauses exp) (cdr exp))
(define (predicate-clause clause) (car clause))
(define (actions-clause clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-else-clause? clause) (eq? (predicate-clause clause) 'else))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (if (cond-else-clause? (car clauses))
          (if (null? (cdr clauses))
              (sequence->exp (actions-clause (car clauses)))
              (error "else is not last"))
          (make-if
           (predicate-clause (car clauses))
           (sequence->exp (actions-clause (car clauses)))
           (expand-clauses (cdr clauses))))))


(cond->if '(cond ((eq? 1 2) 1) ((eq? 2 3) 2) (else 3)))