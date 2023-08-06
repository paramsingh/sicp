#lang sicp
;;; Exercise 2.73: Section 2.3.2 described a program that performs symbolic differentiation:
;;; (define (deriv exp var)
;;;   (cond ((number? exp) 0)
;;;         ((variable? exp)
;;;          (if (same-variable? exp var) 1 0))
;;;         ((sum? exp)
;;;          (make-sum (deriv (addend exp) var)
;;;                    (deriv (augend exp) var)))
;;;         248
;;;         ((product? exp)
;;;          (make-sum (make-product
;;;                     (multiplier exp)
;;;                     (deriv (multiplicand exp) var))
;;;                    (make-product
;;;                     (deriv (multiplier exp) var)
;;;                     (multiplicand exp))))
;;;         ⟨more rules can be added here⟩
;;;         (else (error "unknown expression type:
;;; DERIV" exp))))

;;; We can regard this program as performing a dispatch on
;;; the type of the expression to be differentiated. In this situation the “type tag” of the datum is the algebraic operator
;;; symbol (such as +) and the operation being performed is
;;; deriv. We can transform this program into data-directed
;;; style by rewriting the basic derivative procedure as

(define get '())
(define put '())

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;;; Explain what was done above. Why can’t we assimilate the predicates number? and variable? into the
;;; data-directed dispatch?

;;; ANSWER: With the new method, we use the operator to get the required derivative procedure from the
;;; table instead of defining each of them one by one. Can't do the same for number? and variable? because
;;; they're not operators, instead a different class of selectors.


;;; Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in
;;; the table used by the program above.

(define (variable? e) (symbol? e))

(define (same-variable? e x)
  (and (variable? e) (variable? x) (eq? e x)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend e) (cadr e))

(define (augend e)
  (cond ((= (length e) 3) (caddr e))
        (else (cons'+ (cddr e)))))

(define (derive-sum exp var)
  (make-sum
   (deriv (addend exp) var)
   (deriv (augend exp) var)))

(define (install-addition)
  (put 'deriv '+ derive-sum))