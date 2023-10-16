#lang racket

(require racket/stream)

(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))

(define (mul-streams s1 s2)
  (stream-map2 * s1 s2))

(define factorials
  (stream-cons 1
               (mul-streams
                factorials
                (in-naturals 2))))

(stream->list (stream-take factorials 10))