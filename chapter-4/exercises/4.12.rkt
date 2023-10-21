#lang sicp


;;; Exercise 4.12:The procedures set-variable-value!,
;;; define-variable! and lookup-variable-value can be expressed
;;; in terms of more abstract procedures for traversing the
;;; environment structure. Define abstractions that capture the
;;; common patterns and redefine the three procedures in terms
;;; of these abstractions

;;; tbf, kind of uglier than previous version
(define (scan-env-and-apply env var found-fn)
  (define (scan vars vals)
    (cond ((null? vars) (scan-env-and-apply (enclosing-environment env) var))
          ((eq? (car vars) var) (found-fn vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan
       (frame-variables (first-frame env))
       (frame-values (first-frame env)))))

(define (lookup-variable-value var env)
  (scan-env-and-apply env var (lambda (vals) (car vals))))

(define (set-variable-value! var val env)
  (scan-env-and-apply env var (lambda (vals) (set-car! vals val))))

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

(define env (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
(lookup-variable-value 'a env)
(define-variable! 'c 5 env)
(display env)
(newline)
(lookup-variable-value 'c env)
(set-variable-value! 'c 20 env)
(lookup-variable-value 'c env)