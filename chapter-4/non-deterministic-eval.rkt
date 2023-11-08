#lang sicp

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
(define (make-quote text) (list 'quote text))

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
(define (cond-else-clause? clause) (eq? (predicate-clause clause) 'else))
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


;;;;;;;;;; Environments
(define (lookup-variable-value var env)
  (define (scan-env env)
    (define (scan vars vals)
      (cond ((null? vars) (scan-env (enclosing-environment env)))
            ((eq? (car vars) var) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan
         (frame-variables (first-frame env))
         (frame-values (first-frame env)))))
  (scan-env env))

(define (set-variable-value! var val env)
  (define (scan-env env)
    (define (scan vars vals)
      (cond ((null? vars) (scan-env (enclosing-environment env)))
            ((eq? (car vars) var) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: set!" var)
        (scan
         (frame-variables (first-frame env))
         (frame-values (first-frame env)))))
  (scan-env env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (> (length vars) (length vals))
          (error "too many arguments supplied")
          (error "too few arguments supplied"))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? proc) (tagged-list? proc 'procedure))
(define (procedure-body proc) (caddr proc))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-environment proc) (cadddr proc))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (meta-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (make-thunk exp env)
  (list 'thunk exp env))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (evaluated-thunk-result thunk)
  (cadr thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-car! (cddr obj) '())
           result))
        ((evaluated-thunk? obj)
         (evaluated-thunk-result obj))
        (else obj)))

(define (meta-apply procedure args env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-values args env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-values args env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure typ: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (delay-it exp env)
  (make-thunk exp env))

(define (list-of-delayed-values exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-values (rest-operands exps) env))))

(define (true? t) (not (eq? t false)))
(define (false? x) (eq? x false))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define primitive-procedures
  (list
   (list 'null? null?)
   (list 'eq? eq?)
   (list '+ +)
   (list '* *)
   (list '= =)
   (list '- -)
   (list 'list list)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt ";;; input: ")
(define output-prompt ";;; value: ")
(define (prompt-for-input prompt)
  (newline)
  (newline)
  (display prompt)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))
(driver-loop)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))


;;; THESE ARE DEFINITIONS TO PUT IN TO TEST THE LAZY
;;; EVALUATION:
;;; TODO: Put these in the environment by default
;;; (define (cons x y) (lambda (m) (m x y)))
;;; (define (car z) (z (lambda (p q) p)))
;;; (define (cdr z) (z (lambda (p q) q)))
;;; (define (list-ref items n)
;;;   (if (= n 0)
;;;       (car items)
;;;       (list-ref (cdr items) (- n 1))))
;;; (define (map proc items)
;;;   (if (null? items)
;;;       '()
;;;       (cons (proc (car items)) (map proc (cdr items)))))
;;; (define (scale-list items factor)
;;;   (map (lambda (x) (* x factor)) items))
;;; (define (add-lists list1 list2)
;;;   (cond ((null? list1) list2)
;;;         ((null? list2) list1)
;;;         (else (cons (+ (car list1) (car list2))
;;;                     (add-lists (cdr list1) (cdr list2))))))
;;; (define ones (cons 1 ones))
;;; (define integers (cons 1 (add-lists ones integers)))
