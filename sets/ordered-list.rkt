#lang racket

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set? x  (car set)))
    ))

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((= (car set1) (car set2)) (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
    ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
    (else (intersection-set set1 (cdr set2)))
    ))

;;; (intersection-set (list 1 2 3 5) (list 1 2 5 6))

;;; Exercise 2.61
(define (adjoin-set x set)
  (define (adjoin-set-helper begin end x)
    (cond
      ((null? end) (append begin (list x)))
      ((= x (car end)) (append begin end))
      ((> x (car end)) (adjoin-set-helper (append begin (list (car end))) (cdr end) x))
      (else (append begin (cons x end)))
      ))
  (adjoin-set-helper '() set x)
  )

;;; (adjoin-set 2 (list 1 4 5 6))
;;; (adjoin-set 4 (list 1 4 5 6))
;;; (adjoin-set 7 (list 1 4 5 6))

;;; Exercise 2.62
(define (union-set set1 set2)
  (define (union-set-helper set1 set2 accum)
    (cond
      ((null? set1) (append accum set2))
      ((null? set2) (append accum set1))
      ((= (car set1) (car set2)) (union-set-helper (cdr set1) (cdr set2) (append accum (list (car set1)))))
      ((< (car set1) (car set2)) (union-set-helper (cdr set1) set2 (append accum (list (car set1)))))
      (else (union-set-helper set1 (cdr set2) (append accum (list (car set2)))))
      ))
  (union-set-helper set1 set2 '())
  )

;;; (union-set (list 1 2 3) (list 4 5 6))
;;; (union-set (list 1 2 3) (list 1 2 4 5 6))
;;; (union-set (list 1 2 3 6 7) (list 1 2 4 5 6))