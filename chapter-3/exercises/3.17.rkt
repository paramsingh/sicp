#lang racket

;;; Exercise 3.17: Devise a correct version of the count-pairs
;;; procedure of Exercise 3.16 that returns the number of distinct
;;; pairs in any structure. (Hint: Traverse the structure,
;;; maintaining an auxiliary data structure that is used to keep
;;; track of which pairs have already been counted.)

(define (count-pairs lst)
  (let ((s (list)))
    (define (helper l)
      (if (or (not (pair? l)) (memq l s))
          0
          (begin
            (set! s (cons l s))
            (+
             (helper (car l))
             (helper (cdr l))
             1))))
    (helper lst)))


(count-pairs '((1 2) (3 4)))