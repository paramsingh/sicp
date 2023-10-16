#lang racket

(require racket/stream)

;;; Exercise 3.55: Define a procedure partial-sums that takes
;;; as argument a stream S and returns the stream whose elements are
;;; S0, S0+S1, S0+S1+S2, . . ..
;;; For example, (partialsums integers) should be the
;;; stream 1, 3, 6, 10, 15, . . ..


(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))


(define (add-streams s1 s2)
  (stream-map2 + s1 s2))

(define (partial-sums s)
  (stream-cons
   (stream-first s)
   (add-streams (stream-rest s) (partial-sums s))))

(stream->list (stream-take (partial-sums (in-naturals 1)) 10))