#lang sicp

;;; Exercise 3.22: Instead of representing a queue as a pair of
;;; pointers, we can build a queue as a procedure with local
;;; state. The local state will consist of pointers to the beginning
;;; and the end of an ordinary list. Thus, the make-queue
;;; procedure will have the form
;;; (define (make-queue)
;;;   (let ((front-ptr . . . )
;;;         (rear-ptr . . . ))
;;;     ⟨definitions of internal procedures⟩
;;;     (define (dispatch m) . . .)
;;;     dispatch))
;;; Complete the definition of make-queue and provide implementations of the queue operations using this representation

(define (make-queue)
  (let ((front '())
        (rear '()))
    (define (front-ptr) front)
    (define (rear-ptr) rear)
    (define (set-front-ptr! item)
      (set! front item))
    (define (set-rear-ptr! item)
      (set! rear item))
    (define (empty-queue?)
      (null? front))
    (define (front-queue)
      (if (empty-queue?)
          (error "queue is empty")
          (car (front-ptr))))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (begin
                 (set-front-ptr! new-pair)
                 (set-rear-ptr! new-pair)))
              (else
               (begin
                 (set-cdr! (rear-ptr) new-pair)
                 (set-rear-ptr! new-pair))))))

    (define (delete-queue!)
      (cond ((empty-queue?) (error "cannot delete from empty queue"))
            (else
             (begin
               (set-front-ptr! (cdr (front-ptr)))
               (if (null? (front-ptr)) (set-rear-ptr! nil))))))
    (define (print-queue)
      (display (front-ptr))
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print) print-queue)
            (else (error "method not supported"))))
    dispatch))

(define q (make-queue))

((q 'insert-queue!) 'a)
((q 'print))
((q 'insert-queue!) 'b)
((q 'print))
((q 'delete-queue!))
((q 'print))
((q 'delete-queue!))
((q 'print))

;;; (a)
;;; (a b)
;;; (b)
;;; ()