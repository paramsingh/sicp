#lang racket
;;; Exercise 3.58: Give an interpretation of the stream computed by the following procedure:
;;; (define (expand num den radix)
;;;     (cons-stream
;;;         (quotient (* num radix) den)
;;;         (expand (remainder (* num radix) den) den radix)))
;;;
;;; (quotient is a primitive that returns the integer quotient of
;;; two integers.)
;;; What are the successive elements produced
;;; by (expand 1 7 10)? What is produced by (expand 3 8 10)?


(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream->list (stream-take (expand 1 7 10) 10))
(stream->list (stream-take (expand 3 8 10) 3))