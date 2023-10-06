#lang sicp

;;; Exercise 3.47: A semaphore (of size n) is a generalization of
;;; a mutex. Like a mutex, a semaphore supports acquire and
;;; release operations, but it is more general in that up to n
;;; processes can acquire it concurrently. Additional processes
;;; that attempt to acquire the semaphore must wait for release
;;; operations. Give implementations of semaphores
;;;
;;; a. in terms of mutexes
;;; b. in terms of atomic test-and-set! operations.

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true
      (begin (set-car! cell true) false)))


(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex ''acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-semaphore-with-mutex n)
  (let ((count 0)
        (mutex (make-mutex)))
    (define (the-semaphore cmd)
      (cond ((eq? cmd 'acquire)
             (mutex 'acquire)
             (if (< count n)
                 (begin (set! count (+ 1 count))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))))
            ((eq? cmd 'release)
             (begin (mutex 'acquire)
                    (set! count (- 1 count))
                    (mutex 'release)))))
    the-semaphore))

(define (make-semaphore-with-test-and-set n)
  (let ((cell (list false))
        (count 0))
    (define (the-semaphore cmd)
      (cond ((eq? cmd 'acquire)
             (cond ((test-and-set! cell)
                    (the-semaphore 'acquire))
                   ((< count n)
                    (set! count (+ count 1))
                    (clear! cell))
                   (else
                    (clear! cell)
                    (the-semaphore 'acquire))))
            ((eq? cmd 'release)
             (cond ((test-and-set! cell) (the-semaphore 'release))
                   ((> count 0)
                    (set! count (- count 1))
                    (clear! cell))
                   (else (clear! cell))))))
    the-semaphore))