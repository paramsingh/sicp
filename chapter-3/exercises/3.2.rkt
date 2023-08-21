#lang sicp

;;; Exercise 3.2: In soware-testing applications, it is useful
;;; to be able to count the number of times a given procedure
;;; is called during the course of a computation. Write a procedure
;;; make-monitored that takes as input a procedure, f,
;;; that itself takes one input. The result returned by make-monitored is
;;; a third procedure, say mf, that keeps track
;;; of the number of times it has been called by maintaining
;;; an internal counter. If the input to mf is the special symbol
;;; how-many-calls?, then mf returns the value of the counter.
;;; If the input is the special symbol reset-count, then mf resets the
;;; counter to zero. For any other input, mf returns the
;;; result of calling f on that input and increments the counter

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls) call-count)
            ((eq? arg 'reset-count)
             (begin (set! call-count 0) call-count))
            (else
             (begin (set! call-count (+ call-count 1))
                    (f arg)))))))

(define monitored-sqrt (make-monitored sqrt))

(monitored-sqrt 100)
(monitored-sqrt 100)
(monitored-sqrt 100)
(monitored-sqrt 'how-many-calls)

(monitored-sqrt 25)
(monitored-sqrt 'how-many-calls)


(monitored-sqrt 'reset-count)
(monitored-sqrt 'how-many-calls)
(monitored-sqrt '25)
(monitored-sqrt 'how-many-calls)


;;; param@Params-MacBook-Pro exercises % racket 3.2.rkt
;;; 10
;;; 10
;;; 10
;;; 3
;;; 5
;;; 4
;;; 0
;;; 0
;;; 5
;;; 1