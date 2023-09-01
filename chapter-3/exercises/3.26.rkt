#lang sicp

;;; Exercise 3.26: To search a table as implemented above, one
;;; needs to scan through the list of records. This is basically
;;; the unordered list representation of Section 2.3.3. For large
;;; tables, it may be more efficient to structure the table in
;;; a different manner. Describe a table implementation where the
;;; (key, value) records are organized using a binary tree, assuming
;;; that keys can be ordered in some way (e.g., numerically or
;;; alphabetically).
;;;                                                                                                                                                       2.)

(define (make-table) (list '() '() '()))

(define (make-node val) (list '() val '()))
(define (left node) (car node))
(define (right node) (caddr node))
(define (node-value node) (cadr node))

(define (set-node-value! node val) (set-car! (cdr node) val))

(define (lookup key table)
  (cond ((null? table) #f)
        ((= key (car (node-value table))) (cdr (node-value table)))
        ((< key (car (node-value table))) (lookup key (left table)))
        ((> key (car (node-value table))) (lookup key (right table)))))

(define (insert! key value table)
  (cond ((null? (cadr table))
         (begin
           (set-car! (cdr table) (cons key value))))
        ((= key (car (node-value table)))
         (begin
           (set-node-value! table (cons key value))))
        ((< key (car (node-value table)))
         (begin
           (if (null? (left table))
               (set-car! table (make-node (cons key value)))
               (insert! key value (left table)))))
        ((> key (car (node-value table)))
         (begin
           (if (null? (right table))
               (set-car! (cddr table) (make-node (cons key value)))
               (insert! key value (right table)))))))

(define table (make-table))
(insert! 3 30 table)
(insert! 2 20 table)
(insert! 1 10 table)
(display table)
(newline)
(insert! 4 40 table)
(insert! 5 50 table)
(display table)
(newline)
(lookup 1 table)
(lookup 2 table)
(lookup 3 table)
(lookup 4 table)
(lookup 5 table)