#lang racket

(require "../engine.rkt")
(require "../nn.rkt")


(define example-xs (list
                    (list 2.0 3.0 -1.0)
                    (list 3.0 -1.0 0.5)
                    (list 0.5 1.0 1.0)
                    (list 1.0 1.0 -1.0)))

(define example-ys (list 1.0 -1.0 -1.0 1.0))


(define n (make-perceptron 3 (list 4 4 1)))

(define (loss ypreds ys)
  (define yvals (map (lambda (v) (make-value v (set) null)) ys))
  (define (helper rpreds ry acc)
    (cond ((empty? rpreds) acc)
          (else
           (let ([diff (sub-value (car rpreds) (car ry))])
             (helper (cdr rpreds) (cdr ry) (add-value
                                            acc
                                            (mul-value diff diff)))))))
  (helper ypreds yvals (make-value 0.0 (set) null)))


(define learning-rate 0.01)

(define (step net xs ys)
  (zero-grad-perceptron! net)
  (define ypreds (map (lambda (x) (car (act-perceptron net x))) xs))
  (define current-loss (loss ypreds ys))
  (backward current-loss)
  (define (update-param p) (set-value-data! p (- (value-data p) (* learning-rate (value-grad p)))) '())
  (for-each (lambda (param) (update-param param)) (parameters-perceptron net))
  (display "current-loss: ")
  (print-value current-loss)
  '())

(for-each (lambda (_) (step n example-xs example-ys)) (make-list 500 '()))
(define ypreds (map (lambda (x) (car (act-perceptron n x))) example-xs))
(for-each print-value ypreds)