#lang racket

(require racket/set)

(struct value (data children grad backward label) #:mutable)

(define (make-value data children label)
  (define _backward (lambda () '()))
  (value (* 1.0 data) children 0.0 _backward label))

(define (print-value val)
  (let ((data (value-data val)))
    (display "label: ")
    (display (value-label val))
    (display ", value: ")
    (display data)
    (display ", grad: ")
    (display (value-grad val))
    (newline)))

(define (add-value val1 val2)
  (define out (make-value (+
                           (value-data val1)
                           (value-data val2)) (set val1 val2) null))
  (define _backward
    (lambda ()
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
                           (value-data val2)) (set val1 val2) null))
  (define _backward
    (lambda ()
      (set-value-grad!
       val1
       (+ (value-grad val1) (* (value-grad out) (value-data val2))))
      (set-value-grad!
       val2
       (+ (value-grad val2) (* (value-grad out) (value-data val1))))))

  (set-value-backward! out _backward)
  out)

(define (backward val)
  (define (topological-sort node)
    (define topo (list))
    (define visited (mutable-set))

    (define (helper v)
      (when (not (set-member? visited v))
        (set-add! visited v)
        (for-each helper (set->list (value-children v)))
        (set! topo (append topo (list v)))))

    (helper node)
    topo)

  (set-value-grad! val 1.0)
  (define order (reverse (topological-sort val)))
  (define (f val) ((value-backward val)))
  (for-each f order)
  )

;;; Testing
(define a (make-value 2 (set) 'a))
(define b (make-value 3 (set) 'b))

(define c (add-value a b))
(set-value-label! c 'c)

(backward c)

(print-value a)
(print-value b)
(print-value c)

(display "===================================")
(newline)

(set-value-grad! a 0.0)
(set-value-grad! b 0.0)
(define d (mul-value a b))
(set-value-label! d 'd)
(backward d)

(print-value a)
(print-value b)
(print-value d)