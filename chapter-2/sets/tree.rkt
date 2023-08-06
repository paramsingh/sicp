#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set)) (element-of-set? x (left-branch set)))
    (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
    (else
     (make-tree (entry set) (right-branch set) (adjoin-set x (right-branch set))))))

(define (tree-to-list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (partial-tree elements n)
  (cond
    ((= n 0) (cons '() elements))
    (else
     (let* ((left-size (quotient (- n 1) 2))
            (left-result (partial-tree elements left-size))
            (left-tree (car left-result))
            (remaining (cdr left-result))
            (right-size (- (- n 1) left-size))
            (right-result (partial-tree (cdr remaining) right-size))
            (right-tree (car right-result))
            (right-remaining (cdr right-result)))
       (cons (make-tree (car remaining) left-tree right-tree) right-remaining)))))



(define (list-to-tree elements)
  (car (partial-tree elements (length elements))))

(tree-to-list (list-to-tree (list 1 2 3 4)))


(list-to-tree (list 1 2 3 4 5 6 8 9))

(define (union-list list1 list2)
  (define (union-set-helper list1 list2 accum)
    (cond
      ((null? list1) (append accum list2))
      ((null? list2) (append accum list1))
      ((= (car list1) (car list2)) (union-set-helper (cdr list1) (cdr list2) (append accum (list (car list1)))))
      ((< (car list1) (car list2)) (union-set-helper (cdr list1) list2 (append accum (list (car list1)))))
      (else (union-set-helper list1 (cdr list2) (append accum (list (car list2)))))
      ))
  (union-set-helper list1 list2 '())
  )

(define (intersection-list list1 list2)
  (cond
    ((or (null? list1) (null? list2)) '())
    ((= (car list1) (car list2)) (cons (car list1) (intersection-list (cdr list1) (cdr list2))))
    ((< (car list1) (car list2)) (intersection-list (cdr list1) list2))
    (else (intersection-list list1 (cdr list2)))
    ))

;; Exercise 2.65
(define (union-set set1 set2)
  (define list1 (tree-to-list set1))
  (define list2 (tree-to-list set2))
  (list-to-tree (union-list list1 list2)))

(union-set (list-to-tree (list 1 2 3)) (list-to-tree (list 2 5 6)))

(define (intersection-set set1 set2)
  (define list1 (tree-to-list set1))
  (define list2 (tree-to-list set2))
  (list-to-tree (intersection-list list1 list2)))

(intersection-set (list-to-tree (list 1 2 3 5 6)) (list-to-tree (list 2 5 6)))