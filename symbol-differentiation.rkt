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

(define (sum? exp) (and (list? exp) (eq? (car exp) '+)))

(define (product? exp) (and (list? exp) (eq? (car exp) '*)))

(define (exponentation? exp) (and (pair? exp) (eq? (car exp) '**)))

(define (make-exponentation a b)
  (cond ((=number? b 0) 1)
        ((=number? a 0) 0)
        ((=number? a 1) 1)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (expt a b)))
        (else (list '** a b))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (addend e) (cadr e))

(define (augend e)
  (cond ((= (length e) 3) (caddr e))
        (else (cons'+ (cddr e)))))

(define (multiplier e) (cadr e))

(define (multiplicand e)
  (cond ((= (length e) 3) (caddr e))
        (else (cons '* (cddr e)))))


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
        ((exponentation? exp)
         (let ([n (exponent exp)]
               [a (base exp)])
           (make-product (make-product n (make-exponentation a (- n 1))) (derive a var))))
        (else
         (error "unknown expression type: DERIV" exp)
         )))

(println (derive '(+ x 3) 'x))
(println (derive '(* x y) 'x))
(pretty-print (derive '(* (* x y) (+ x 3)) 'x))
(println (derive '(** x 3) 'x))
(pretty-print (derive '(* x y (+ x 3)) 'x))
(pretty-print (derive '(+ x (** x 2)) 'x))

;;; 2.58 asks you to support infix with parantheses and only two operands. This is easy because
;;; racket does the parsing of the expression into nested lists for us.

;;; if the parantheses did not exist, we'd have to convert from infix to prefix and then parse it.