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

(define (pow-value val1 x)
  (define out (make-value (expt (value-data val1) x) (set val1) null))
  (define _backward
    (lambda ()
      (set-value-grad!
       val1
       (+
        (value-grad val1)
        (* (* x (expt (value-data val1) (- x 1))) (value-grad out))))))

  (set-value-backward! out _backward)
  out)

(define (sub-value val1 val2)
  (add-value val1 (mul-value val2 (make-value -1 (set) null))))

(define (div-value val1 val2)
  (mul-value val1 (pow-value val2 -1)))

(define (relu-value val)
  (define out (make-value (if (< (value-data val) 0) 0 (value-data val))))
  (define _backward
    (lambda ()
      (set-value-grad! val
                       (+ (value-data val)
                          (* (if (> (value-data out) 0) 1 0) (value-grad out))))))
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

(provide (all-defined-out))