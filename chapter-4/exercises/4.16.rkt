#lang sicp

(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (first-frame env) (car env))

(define (lookup-variable-value var env)
  (define (scan-env env)
    (define (scan vars vals)
      (cond ((null? vars) (scan-env (enclosing-environment env)))
            ((eq? (car vars) var)
             (if (eq? (car vals) '*unassigned*)
                 (error "variable is unassigned" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan
         (frame-variables (first-frame env))
         (frame-values (first-frame env)))))
  (scan-env env))

;;; quotations have the form ('quote <text of quotation>)
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-let pairs body)
  (list 'let pairs body))
(define (make-set! var value)
  (list 'set! var value))
(define (make-quote text) (list 'quote text))

(define (scan-out-defines lambda-exp)

  (define (get-define-vars b)
    (display b)
    (newline)
    (cond ((null? b) '())
          ((definition? (car b))
           (cons (definition-variable (car b))
                 (get-define-vars (cdr b))))
          (else (get-define-vars (cdr b)))))

  (define (set-to-undefined variables)
    (cond ((null? variables) '())
          (else (cons (list (car variables) (make-quote '*undefined*))
                      (set-to-undefined (cdr variables))))))

  (define (get-body-with-set! b)
    (cond ((null? b) '())
          ((definition? (car b))
           (cons
            (make-set! (definition-variable (car b)) (definition-value (car b)))
            (get-body-with-set! (cdr b))))
          (else (cons (car b) (get-body-with-set! (cdr b))))))


  (let ((defined-variables (get-define-vars (lambda-body lambda-exp))))
    (display "defined variables: ")
    (display defined-variables)
    (newline)
    (make-lambda (lambda-parameters lambda-exp)
                 (list (make-let
                        (set-to-undefined defined-variables)
                        (get-body-with-set! (lambda-body lambda-exp)))))))

(scan-out-defines '(lambda (x) (define y (+ 1 2)) (define z (+ 3 4)) (+ y z)))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))