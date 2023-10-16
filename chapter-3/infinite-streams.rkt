#lang racket

(require racket/stream)


;;; ;;; Integers from n
;;; (define (integers-from-n n)
;;;   (stream-cons n (integers-from-n (+ n 1))))

;;; (define integers (integers-from-n 1))

;;; (define (divisible? x y) (= (remainder x y) 0))


;;; (define no-sevens
;;;   (stream-filter (lambda (x) (not (divisible? x 7)))
;;;                  integers))

;;; (stream-ref no-sevens 100)

;;; ;;; Fibonacci
;;; (define (fibgen a b) (stream-cons a (fibgen b (+ a b))))

;;; (define fibs (fibgen 0 1))

;;; (stream-ref fibs 4)

;;; ;;; prime seive
;;; (define (seive stream)
;;;   (stream-cons (stream-first stream)
;;;                (seive
;;;                 (stream-filter (lambda (x) (not (divisible? x (stream-first stream))))
;;;                                (stream-rest stream)))))

;;; (define primes (seive (integers-from-n 2)))

;;; (stream-ref primes 2)
;;; (stream-ref primes 3)
;;; (stream-ref primes 5)


(define ones (stream-cons 1 ones))

(define (stream-map2 f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      (stream)
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-map2 f (stream-rest s1) (stream-rest s2)))))


(define (add-streams s1 s2)
  (stream-map2 + s1 s2))

(define integers-again (stream-cons 1 (add-streams ones integers-again)))

(stream-ref integers-again 100)

(define fibs (stream-cons 0 (stream-cons 1 (add-streams fibs (stream-rest fibs)))))
(stream-ref fibs 4)
(stream-ref fibs 5)
(stream-ref fibs 6)
(stream-ref fibs 7)
(stream-ref fibs 8)

(define s (stream-cons 1 (add-streams s s)))

(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)