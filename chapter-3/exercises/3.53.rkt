#lang racket
(require racket/stream)



(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))


(define (add-streams s1 s2)
  (stream-map2 + s1 s2))


(define s (stream-cons 1 (add-streams s s)))
;;; 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 ...