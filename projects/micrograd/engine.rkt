#lang racket

(require racket/set)

(struct value (data children grad backward) #:mutable)

(define (make-value data children)
  (define _backward (lambda () '()))
  (value (* 1.0 data) children 0.0 _backward))

(define (print-value val)
  (let ((data (value-data val)))
    (display "value: ")
    (display data)
    (display ", grad: ")
    (display (value-grad val))
    (newline)))

(define (add-value val1 val2)
  (define out (make-value (+
                           (value-data val1)
                           (value-data val2)) (set val1 val2)))
  (define _backward (lambda ()
                      (set-value-grad!
                       val1
                       (+ (value-grad val1) (value-grad out)))
                      (set-value-grad!
                       val2
                       (+ (value-grad val2) (value-grad out)))))

  (set-value-backward! out _backward)

  out)

(define (mul-value val1 val2)
  (define out (make-value (*
                           (value-data val1)
                           (value-data val2)) (set val1 val2)))
  (define _backward (lambda ()
                      (set-value-grad!
                       val1
                       (+ (value-grad val1) (* (value-grad out) (value-data val2))))
                      (set-value-grad!
                       val2
                       (+ (value-grad val2) (* (value-grad out) (value-data val1))))))

  (set-value-backward! out _backward)
  out)


(define a (make-value 2 (set)))
(define b (make-value 3 (set)))

(define c (add-value a b))

(define (topological-sort val)
  (define children (set->list (value-children val)))
  (if (empty? children)
      (list)
      (filter
       (lambda (x) (not (null? x)))
       (append children (map topological-sort children)))))

(define (backward val)
  (set-value-grad! val 1.0)
  (define order (append (list val) (topological-sort val)))
  (define (f val) ((value-backward val)))
  (for-each f order)
  )

(backward c)

(print-value a)
(print-value b)
(print-value c)

(display "===================================")
(newline)

(set-value-grad! a 0.0)
(set-value-grad! b 0.0)
(define d (mul-value a b))
(backward d)

(print-value a)
(print-value b)
(print-value d)