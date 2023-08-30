#lang sicp

;;; Exercise 3.18: Write a procedure that examines a list and
;;; determines whether it contains a cycle, that is, whether a
;;; program that tried to find the end of the list by taking successive
;;; cdrs would go into an infinite loop. Exercise 3.13
;;; constructed such lists

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cyclic? lst)
  (let ((encountered (list)))
    (define (helper l)
      (cond ((null? l) false)
            ((memq (car l) encountered) true)
            (else (begin
                    (set! encountered (cons (car l) encountered))
                    (helper (cdr l))))))
    (helper lst)))

(cyclic? (list 1 2 3))
(cyclic? z)