#lang sicp

;;; Exercise 3.24: In the table implementations above, the keys
;;; are tested for equality using equal? (called by assoc). is
;;; is not always the appropriate test. For instance, we might
;;; have a table with numeric keys in which we don’t need an
;;; exact match to the number we’re looking up, but only a
;;; number within some tolerance of it. Design a table constructor
;;; make-table that takes as an argument a same-key?
;;; procedure that will be used to test “equality” of keys.
;;; make-table should return a dispatch procedure that can be used
;;; to access appropriate lookup and insert! procedures for a
;;; local table.

(define (assoc predicate key records)
  (cond ((null? records) false)
        ((predicate key (caar records)) (car records))
        (else (assoc predicate key (cdr records)))))

(define (make-table same-key-predicate)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc same-key-predicate key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc same-key-predicate key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc same-key-predicate key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc same-key-predicate key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1 2 'a)
(get 1 2)