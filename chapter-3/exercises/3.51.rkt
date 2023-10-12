#lang sicp


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x) (newline) (display x))

;;; (define (force delayed-object)
;;;   (delayed-object))

(define (memo-proc f)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (f))
                 (set! already-run? true)
                 result)
          result))))
;;; (define memo-f (memo-proc (lambda () 10)))
;;; (display memo-f)
;;; (newline)
;;; (display (memo-f))
;;; (newline)



;;; (define (delay exp) (lambda () ((memo-proc (lambda () exp)))))

;;; (define blah (delay (lambda () 10)))
;;; (display blah)
;;; (newline)
;;; (newline)
;;; (newline)
;;; (display "forcing")
;;; (force blah)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))


(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show (stream-enumerate-interval 0 10)))
(define fifth (stream-ref x 5))
(newline)
(define seventh (stream-ref x 7))
(newline)