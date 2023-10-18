#lang racket

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

(define (triples a b c)
  (if (stream-empty? a)
      (empty-stream)
      (stream-cons
       (list (stream-first a) (stream-first b) (stream-first c))
       (if (stream-empty? (stream-rest c))
           (if (stream-empty? (stream-rest b))
               (triples (stream-rest a) (stream-rest a) (in-naturals 1))
               (triples a (stream-rest b) (stream-rest b)))
           (triples a b (stream-rest c))))))

(define natural-triples (triples (in-naturals 1) (in-naturals 1) (in-naturals 1)))


;;; (define (triples a b c)
;;;   (define (flatten s1)
;;;     (let ((first-pair (car (stream-first s1))))
;;;       (stream-cons
;;;        (list (car first-pair) (cadr first-pair) (cadr (stream-first s1)))
;;;        (flatten (stream-rest s1)))))
;;;   (flatten (pairs (pairs a b) c)))


;;; (define natural-triples (triples (in-naturals 1) (in-naturals 1) (in-naturals 1)))
;;;
(define (check triplet)
  (let ((a (car triplet))
        (b (cadr triplet))
        (c (caddr triplet)))
    (= (+ (* a a) (* b b)) (* c c))))

;;; (check (list 3 4 5))

(define pythagorean-triples
  (stream-filter (lambda (x) (check x)) natural-triples))

(stream->list (stream-take pythagorean-triples 1))