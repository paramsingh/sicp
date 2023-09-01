#lang sicp

;;; Exercise 3.23:A deque (“double-ended queue”) is a sequence
;;; in which items can be inserted and deleted at either the
;;; front or the rear. Operations on deques are the constructor
;;; make-deque, the predicate empty-deque?,
;;; selectors front-deque and rear-deque, mutators front-insert-deque!,
;;; rear-insert-deque!, front-delete-deque!, and rear-delete-deque!.
;;; Show how to represent deques using pairs, and
;;; give implementations of the operations. All operations
;;; should be accomplished in Θ(1) steps.

(define (make-node val) (list '() val '()))
(define (node-value node) (cadr node))

(define (next node)
  (if (null? (cdr node))
      nil
      (caddr node)))

(define (previous node) (car node))
(define (set-previous! node prev)
  (set-car! node prev))

(define (set-next! node nxt)
  (set-car! (cddr node) nxt))


(define (set-front-node! dq node)
  (set-car! dq node))

(define (set-rear-node! dq node)
  (set-cdr! dq node))

(define (rear-node-deque dq)
  (cdr dq))

(define (rear-deque dq)
  (node-value (rear-node-deque dq)))

(define (front-node-deque dq)
  (car dq))

(define (front-deque dq)
  (node-value (car dq)))

(define (empty-dq? dq)
  (null? (front-deque dq)))

(define (make-deque)
  (cons (make-node nil) (make-node nil)))

(define (end-of-deque? node)
  (null? (next node)))

(define (rear-insert-deque! dq val)
  (let ((new-node (make-node val)))
    (if (empty-dq? dq)
        (begin (set-front-node! dq new-node)
               (set-rear-node! dq new-node))
        (begin
          (set-previous! new-node (rear-node-deque dq))
          (set-next! (rear-node-deque dq) new-node)
          (set-rear-node! dq new-node)))
    dq))

(define (front-insert-deque! dq val)
  (let ((new-node (make-node val)))
    (if (empty-dq? dq)
        (begin
          (set-front-node! dq new-node)
          (set-rear-node! dq new-node))
        (begin
          (set-next! new-node (front-node-deque dq))
          (set-previous! (front-node-deque dq) new-node)
          (set-front-node! dq new-node)))
    dq))

(define (rear-delete-deque! dq)
  (cond ((empty-dq? dq)
         (error "empty deque, cannot delete"))
        ((not (null? (previous (rear-node-deque dq))))
         (begin
           (set-next! (previous (rear-node-deque dq)) '())
           (set-rear-node! dq (previous (rear-node-deque dq)))
           dq))
        (else
         (let ((new-node (make-node nil)))
           (set-front-node! dq new-node)
           (set-rear-node! dq new-node)
           dq))))

(define (front-delete-deque! dq)
  (cond ((empty-dq? dq)
         (error "empty deque, cannot delete"))
        ((not (null? (next (front-node-deque dq))))
         (begin
           (set-previous! (next (front-node-deque dq)) nil)
           (set-front-node! dq (next (front-node-deque dq)))
           dq))
        (else
         (let ((new-node (make-node nil)))
           (set-front-node! dq new-node)
           (set-rear-node! dq new-node)
           dq))))

(define (print-deque dq)
  (define (helper node)
    (if (end-of-deque? node)
        (begin
          (display (node-value node))
          (newline))
        (begin
          (display (node-value node))
          (display " -> ")
          (helper (next node)))))
  (helper (front-node-deque dq)))


(define dq (make-deque))
(next (front-node-deque dq))
(print-deque (rear-insert-deque! dq 1))
(print-deque (rear-insert-deque! dq 2))
(print-deque (rear-delete-deque! dq))
(print-deque (rear-delete-deque! dq))
(print-deque (rear-insert-deque! dq 1))
(print-deque (rear-insert-deque! dq 10))
(print-deque (rear-insert-deque! dq 20))
(print-deque (front-insert-deque! dq 40))
(print-deque (front-insert-deque! dq 50))
(print-deque (rear-delete-deque! dq))
(print-deque (front-delete-deque! dq))
(print-deque (front-delete-deque! dq))
(print-deque (front-delete-deque! dq))
(print-deque (rear-delete-deque! dq))