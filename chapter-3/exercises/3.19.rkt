#lang sicp
;;; Exercise 3.19: Redo Exercise 3.18 using an algorithm that
;;; takes only a constant amount of space. (This requires a very
;;; clever idea.)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? lst)
  (define (helper slow fast)
    (cond ((eq? slow fast) true)
          ((or (null? fast) (null? (cdr fast))) false)
          (else (helper (cdr slow) (cdr (cdr fast))))))
  (if (or (null? lst) (null? (cdr lst)))
      false
      (helper (cdr lst) (cdr (cdr lst)))))

(cycle? z)
(cycle? (list))
(cycle? (list 1))
(cycle? (list 1 1 2 3 4))