#lang sicp

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))


(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define (filter-stream pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (filter-stream pred (stream-cdr s))))
        (else (filter-stream pred (stream-cdr s)))))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (filter-stream (lambda (x) (not (divisible? x (stream-car s))))
                         (stream-cdr s)))))

(define primes (sieve (integers-from 2)))

; To get the first n primes:
(define (list-n-primes n)
  (if (= n 0) '()
      (cons (stream-car primes)
            (begin (set! primes (stream-cdr primes))
                   (list-n-primes (- n 1))))))

; Example: Get the first 10 primes
(display (list-n-primes 10))
