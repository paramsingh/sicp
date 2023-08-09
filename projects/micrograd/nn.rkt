#lang racket

(require "engine.rkt")

(define (random-weight) (- (* 2 (random)) 1))

;;; neuron
(struct neuron (weights bias) #:mutable)
(define (make-neuron nin)
  (define w
    (build-list nin (lambda (_) (make-value (random-weight) (set) null))))
  (define bias (make-value (random-weight) (set) null))
  (neuron w bias))

(define (parameters-neuron n)
  (append (neuron-weights n) (list (neuron-bias n))))

(define (zero-grad-neuron! n)
  (for-each (lambda (val) (set-value-grad! val 0.0)) (parameters-neuron n)))

(define (act-neuron n x)
  (define (helper rw rx acc)
    (cond ((empty? rw) acc)
          (else (helper (cdr rw) (cdr rx) (add-value acc (mul-value (car rw) (car rx)))))))
  (helper (neuron-weights n) x (neuron-bias n)))

;;; layer
(struct layer (neurons nin nout) #:mutable)

(define (make-layer nin nout)
  (layer (build-list nout (lambda (_) (make-neuron nin))) nin nout))

(define (parameters-layer l)
  (foldl append (list) (map parameters-neuron (layer-neurons l))))

(define (zero-grad-layer! l)
  (for-each zero-grad-neuron! (layer-neurons l)))

(define (act-layer l x)
  (map (lambda (n) (act-neuron n x)) (layer-neurons l)))

(define (print-layer l)
  (display "nin: ")
  (display (layer-nin l))
  (display ", nout: ")
  (display (layer-nout l))
  (newline))

;;; multi-layer-perceptron
(struct multi-layer-perceptron (layers nin nouts) #:mutable)

(define (make-perceptron nin nouts)
  (define (get-layers sizes)
    (cond ((= (length sizes) 1) (list))
          (else (append (list (make-layer (car sizes) (cadr sizes))) (get-layers (cdr sizes))))))
  (multi-layer-perceptron (get-layers (append (list nin) nouts)) nin nouts))

(define (parameters-perceptron p)
  (foldl append (list) (map parameters-layer (multi-layer-perceptron-layers p))))

(define (zero-grad-perceptron! p)
  (for-each zero-grad-layer! (multi-layer-perceptron-layers p)))

(define (act-perceptron p x)
  (define xs (map (lambda (val) (make-value val (set) null)) x))
  (define (apply layers current)
    (cond ((null? layers) current)
          (else (apply (cdr layers) (act-layer (car layers) current)))))
  (apply (multi-layer-perceptron-layers p) xs))

(provide (all-defined-out))