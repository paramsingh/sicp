#lang racket

;;; Exercise 3.64: Write a procedure stream-limit that takes
;;; as arguments a stream and a number (the tolerance). It should
;;; examine the stream until it finds two successive elements
;;; that differ in absolute value by less than the tolerance, and
;;; return the second of the two elements. Using this, we could
;;; compute square roots up to a given tolerance by
;;; (define (sqrt x tolerance)
;;;   (stream-limit (sqrt-stream x) tolerance)


(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ((first (stream-first s))
        (second (stream-first (stream-rest s))))
    (cond
      ((< (abs (- first second)) tolerance) second)
      (else (stream-limit (stream-rest s) tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.000001)
