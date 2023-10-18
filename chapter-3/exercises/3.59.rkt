#lang racket

(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))

;;; 3.59 a
(define (integrate-series power-series)
  (stream-map2
   (lambda (x y) (* x (/ 1 y)))
   (in-naturals 1)
   power-series))

;;; 3.59 b
(define (exp-series)
  (stream-cons 1 (integrate-series exp-series)))

(define (cosine-series)
  (stream-cons 1 (integrate-series sine-series)))
(define (sine-series)
  (stream-cons 0 (integrate-series (stream-map (lambda (x) (- x)) cosine-series))))
