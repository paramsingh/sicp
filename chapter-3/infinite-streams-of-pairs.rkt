#lang racket

(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(stream->list (stream-take (pairs (in-naturals 1) (in-naturals 1)) 50))
