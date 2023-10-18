#lang racket


;;; Exercise 3.60: With power series represented as streams
;;; of coefficients as in Exercise 3.59, adding series is implemented by add-streams. Complete the definition of the following procedure for multiplying series:
;;; (define (mul-series s1 s2)
;;;   (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))
;;; You can test your procedure by verifying that sin^2x + cos^2x = 1
;;; using the series from Exercise 3.59.

(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))

(define (add-streams s1 s2)
  (stream-map2 + s1 s2))

(define (mul-series s1 s2)
  (stream-cons (add-streams ⟨??⟩ ⟨??⟩)))
