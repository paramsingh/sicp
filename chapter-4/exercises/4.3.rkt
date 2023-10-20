#lang sicp

;;; Exercise 4.3: Rewrite eval so that the dispatch is done
;;; in data-directed style. Compare this with the data-directed
;;; differentiation procedure of Exercise 2.73. (You may use the
;;; car of a compound expression as the type of the expression,
;;; as is appropriate for the syntax implemented in this
;;; section.)

;;; only self evaluating things are numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;; symbols are variables
(define (variable? exp) (symbol? exp))

;;; quotations have the form ('quote <text of quotation>)
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;;; assignments look like (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;; definitions have the form (define <var> <value>)
;;; or (define (<var> <parameter1> ... <parametern>)
;;;      <body>)
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (cond ((symbol? (cadr exp)) (cadr exp))
        (else (caadr exp))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition-value exp)
  (cond ((symbol? (cadr exp)) (caddr exp))
        (else (make-lambda (cdadr exp)
                           (cddr exp)))))

;;; lambdas are lists that have the lambda tag
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;;; if  are lists with the if tag
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; begin packages a sequence of expressions into a single expression.
;;; We include syntax operations on begin expressions to extract the actual sequence
;;; from the begin expression, as well as
;;; selectors that return the first expression and the rest of the expressions in the sequence.
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

;;; A procedure application is any compound expression that is not
;;; one of the above expression types. The car of the expression is
;;; the operator, and the cdr is the list of operands:
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;; cond we derive from if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (predicate-clause clause) (car clause))
(define (actions-clause clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-else-clause? clause) (eq? predicate-clause 'else))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (if (cond-else-clause? (car clauses))
          (if (null? (cdr clauses))
              (sequence->exp (actions-clause (car clauses)))
              (error "else is not last"))
          (make-if
           (predicate-clause (car clauses))
           (sequence->exp (actions-clause (car clauses)))
           (expand-clauses (cdr clauses))))))

(define (lookup-variable-value exp env) '())

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (true? t) (eq? t true))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (set-variable-value! a b c) '())
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (define-variable! a b c) '())
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define operation-table (make-table))

(define (op-quote exp env) (text-of-quotation exp))
(insert! 'quote op-quote operation-table)

(insert! 'set! eval-assignment operation-table)
(insert! 'define eval-definition operation-table)

(define (op-if exp env)
  (eval-if exp env))
(insert! 'if op-if operation-table)

(define (op-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
(insert! 'lambda op-lambda operation-table)

(define (op-begin exp env)
  (eval-sequence (begin-actions exp) env))
(insert! 'begin op-begin operation-table)

(define (op-cond exp env)
  (eval (cond->if exp) env))
(insert! 'cond op-cond operation-table)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (((lookup (car exp) operation-table)) ((lookup (car exp) operation-table) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (make-procedure) '())
(define (primitive-procedure? proc) '())
(define (apply-primitive-procedure proc args) '())
(define (compound-procedure? proc) '())
(define (procedure-body proc) '())
(define (procedure-parameters proc) '())
(define (procedure-environment proc) '())
(define (extend-environment a b c) '())
