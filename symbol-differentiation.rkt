#lang racket

(define (variable? e) (symbol? e))

(define (same-variable? e x)
  (and (variable? e) (variable? x) (eq? e x)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product x y)
  (cond ((=number? x 0) 0)
        ((=number? y 0) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y) (* x y)))
        (else (list '* x y))))

(define (sum? exp) (and (pair? exp) (eq? (car exp) '+)))

(define (product? exp) (and (pair? exp) (eq? (car exp) '*)))

(define (addend e) (cadr e))

(define (augend e) (caddr e))

(define (multiplier e) (cadr e))

(define (multiplicand e) (caddr e))

(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (derive (addend exp) var)
                              (derive (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (derive (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (derive (multiplier exp) var))))
        (else
         (error "unknown expression type: DERIV" exp)
         )))

(println (derive '(+ x 3) 'x))
(println (derive '(* x y) 'x))
(pretty-print (derive '(* (* x y) (+ x 3)) 'x))
