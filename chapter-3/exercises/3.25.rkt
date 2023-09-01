#lang sicp

;;; Exercise 3.25: Generalizing one- and two-dimensional tables,
;;; show how to implement a table in which values are
;;; stored under an arbitrary number of keys and different
;;; values may be stored under different numbers of keys. 
;;; The lookup and insert! procedures should take as input a list
;;; of keys used to access the table.

(define (make-table) (list '*table*))
(define (lookup keys table)
  (let ((retrieved
         (assoc (car keys) (cdr table))))
    (cond
      ((eq? retrieved false) false)
      ((null? (cdr keys)) (cdr retrieved))
      (else (lookup (cdr keys) (cdr retrieved))))))


(define (insert! keys value table)
  (let ((retrieved
         (assoc (car keys) (cdr table))))
    (cond
      ;;; if last key and we don't have a record, add one
      ((and (null? (cdr keys)) (eq? retrieved false))
       (set-cdr! table (cons
                        (cons (car keys) value)
                        (cdr table))))
      ;;; if last key and we have a record already, reset it
      ((and (null? (cdr keys)) (not (eq? retrieved false)))
       (set-cdr! retrieved value))
      ;;; if not last key and we don't have a subtable, add one and insert
      ((and (not (null? (cdr keys))) (eq? retrieved false))
       (let ((new-table (make-table)))
         (set-cdr! table (cons
                          (cons (car keys) new-table)
                          (cdr table)))
         (insert! (cdr keys) value new-table)))
      ;;; if not last key and we have a subtable, keep going
      ((and (not (null? (cdr keys))) (not (eq? retrieved true)))
       (insert! (cdr keys) value (cdr retrieved)))))
  'ok)

(define table (make-table))
(insert! (list 'a 'b 'c) 1 table)
(insert! (list 'a 'b 'd) 2 table)
(insert! (list 'a 'b 'e 'f) 10 table)

(lookup (list 'a 'b 'c) table)
(lookup (list 'a 'b 'd) table)
(lookup (list 'a 'b 'e 'f) table)
