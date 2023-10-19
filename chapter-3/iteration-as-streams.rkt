#lang racket


(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(stream->list (stream-take (sqrt-stream 2) 10))