#lang sicp

;;; only self evaluating things are numbers and strings
(define (self-evaluating? exp)
  (cond
    ((null? exp) true)
    ((number? exp) true)
    ((string? exp) true)
    (else false)))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

(define (variable? exp)
  (symbol? exp))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))
(define (first-frame env) (car env))

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

(define (ambeval exp env succeed fail) ((analyze exp) env succeed fail))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (make-quote text) (list 'quote text))

(define (analyze-quote exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed text fail))))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

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

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (valproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (valproc
       env
       (lambda (value fail2)
         (let ((old-value (lookup-variable-value var env)))
           (set-variable-value! var value env)
           (succeed 'ok
                    (lambda ()
                      (set-variable-value! var old-value)
                      (fail2)))))
       fail))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (valproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (valproc
       env
       (lambda (value fail2)
         (define-variable! var value env)
         (succeed 'ok fail2))
       fail))))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      nil))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (true? val) (not (eq? val false)))

(define (analyze-if exp)
  (let ((predicate-proc (analyze (if-predicate exp)))
        (consequent-proc (analyze (if-consequent exp)))
        (alternative-proc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (predicate-proc
       env
       (lambda (predicate-val fail2)
         (if (true? predicate-val)
             (consequent-proc env succeed fail2)
             (alternative-proc env succeed fail2)))
       fail))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp))
        (body (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure params body env) fail))))

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

(define (analyze-sequence exps)

  (define (sequentially first second)
    (lambda (env succeed fail)
      (first
       env
       (lambda (first-val fail2)
         (second env succeed fail2))
       fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))

  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


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

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc
       env
       (lambda (proc fail2)
         (get-args
          aprocs
          env
          (lambda (args fail3)
            (execute-application proc args succeed fail3))
          fail2))
       fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (first-arg fail2)
         (get-args
          (cdr aprocs)
          env
          (lambda (vals fail3)
            (succeed (cons first-arg vals) fail3))
          fail2))
       fail)))

(define primitive-procedures
  (list
   (list 'even? even?)
   (list 'not not)
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
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

(define (compound-procedure? proc) (tagged-list? proc 'procedure))

(define (procedure-body proc) (caddr proc))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-environment proc) (cadddr proc))

(define (make-frame variables values)
  (cons variables values))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (> (length vars) (length vals))
          (error "too many arguments supplied")
          (error "too few arguments supplied"))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (let-param-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (define (parameter-names param-pairs)
    (cond ((null? param-pairs) '())
          (else (cons (caar param-pairs) (parameter-names (cdr param-pairs))))))
  (define (parameter-values param-pairs)
    (cond ((null? param-pairs) '())
          (else (cons (cadar param-pairs) (parameter-values (cdr param-pairs))))))
  (cons
   (make-lambda
    (parameter-names (let-param-pairs exp))
    (list (sequence->exp (let-body exp))))
   (parameter-values (let-param-pairs exp))))

(define (analyze-let exp)
  (let ((combproc (analyze (let->combination exp))))
    combproc))

(define (let? exp) (tagged-list? exp 'let))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((choice-procs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-again choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-again (cdr choices))))))
      (try-again choice-procs))))


(define (permanent-set? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (valproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (valproc
       env
       (lambda (value fail2)
         (set-variable-value! var value env)
         (succeed 'ok fail2))
       fail))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-success-result exp) (cadr exp))
(define (if-fail-failure-result exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((success-proc (analyze (if-fail-success-result exp)))
        (failure-proc (analyze (if-fail-failure-result exp))))
    (lambda (env succeed fail)
      (success-proc
       env
       succeed
       (lambda ()
         (failure-proc env
                       succeed
                       fail))))))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp)
         (analyze-variable exp))
        ((quoted? exp) (analyze-quote exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-set? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let exp))
        ((amb? exp) (analyze-amb exp))
        ((application? exp)
         (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

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
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem")
            (ambeval
             input
             the-global-environment
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             (lambda ()
               (announce-output ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(driver-loop)

;;; (let ((x (an-element-of '(a b c)))
;;;       (y (an-element-of '(a b c))))
;;;   (permanent-set! count (+ count 1))
;;;   (require (not (eq? x y)))
;;;   (list x y count))