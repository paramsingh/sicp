#lang sicp


;;; Exercise 4.8: “Named let” is a variant of let that has the form
;;;
;;; (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
;;;
;;; The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let,
;;; except that ⟨var⟩ is bound within ⟨body⟩ to a procedure whose
;;; body is ⟨body⟩ and whose parameters are the variables in
;;; the ⟨bindings⟩. Thus, one can repeatedly execute the ⟨body⟩
;;; by invoking the procedure named ⟨var⟩. For example, the
;;; iterative Fibonacci procedure (Section 1.2.2) can be rewritten
;;; using named let as follows:
;;;
;;; (define (fib n)
;;;   (let fib-iter ((a 1)
;;;                  (b 0)
;;;                  (count n))
;;;     (if (= count 0)
;;;         b
;;;         (fib-iter (+ a b) a (- count 1)))))
;;;
;;; Modify let->combination of Exercise 4.6 to also support
;;; named let.

;;; ((define (fib-iter a b count) <body>)
;;;  (fib-iter 1 0 n))


(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (let-param-pairs exp)
  (if (not (named-let? exp))
      (cadr exp)
      (caddr exp)))

(define (parameter-names param-pairs)
  (cond ((null? param-pairs) '())
        (else (cons (caar param-pairs) (parameter-names (cdr param-pairs))))))

(define (let-body exp)
  (if (not (named-let? exp))
      (cddr exp)
      (cadddr exp)))

(define (named-let? exp) (symbol? (cadr exp)))

(define (let-name exp) (cadr exp))

(define (make-define name params body)
  (list 'define (cons name params) body))

(define (parameter-values param-pairs)
  (cond ((null? param-pairs) '())
        (else (cons (cadar param-pairs) (parameter-values (cdr param-pairs))))))

(define (make-function-from-named-let exp)
  (list
   (make-define
    ;;; function name
    (let-name exp)
    ;;; parameters
    (parameter-names (let-param-pairs exp))
    ;;; body
    (let-body exp))
   (cons (let-name exp) (parameter-values (let-param-pairs exp)))))

(define (let->combination exp)
  (cond ((named-let? exp)
         (sequence->exp (make-function-from-named-let exp)))
        (else (cons
               (make-lambda
                (parameter-names (let-param-pairs exp))
                (sequence->exp (let-body exp)))
               (parameter-values (let-param-pairs exp))))))

(let->combination '(let ((x 1) (y 2)) (+ x y)))
(let->combination '(let fn ((x 1)) (+ x (fn x))))
(let->combination '(let fib-iter ((a 1) (b 0) (count 2)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
;;; (begin (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter 1 0 10))