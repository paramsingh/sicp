#lang sicp



(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(an-element-of (list 2 3 1))

(define (even-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (even? (+ a b)))
    (list a b)))

(even-pair (list 1 2 3) (list 2 3 4))