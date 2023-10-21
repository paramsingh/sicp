#lang sicp

;;; Exercise 4.11: Instead of representing a frame as a pair of
;;; lists, we can represent a frame as a list of bindings, where
;;; each binding is a name-value pair. Rewrite the environment
;;; operations to use this alternative representation

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
    (define (scan vars vals frame)
      (cond ((null? vars) (scan-env (enclosing-environment env)))
            ((eq? (car vars) var) (set-in-frame! var val frame))
            (else (scan (cdr vars) (cdr vals) frame))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: set!" var)
        (scan
         (frame-variables (first-frame env))
         (frame-values (first-frame env))
         (first-frame env))))
  (scan-env env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-in-frame! var val frame))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cond ((null? variables) '())
        (else (cons
               (cons (car variables) (car values))
               (make-frame (cdr variables) (cdr values))))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))


(define (set-in-frame! var val frame)
  (cond ((null? frame) (error "variable not in frame" var val))
        ((eq? (caar frame) var)
         (set-cdr! (car frame) val))
        (else (set-in-frame! var val (cdr frame)))))


(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (> (length vars) (length vals))
          (error "too many arguments supplied")
          (error "too few arguments supplied"))))


;;; testing frames
(define example-frame (make-frame (list 'a 'b 'c) (list 1 2 3)))
(frame-variables example-frame)
(frame-values example-frame)
(add-binding-to-frame! 'd 5 example-frame)
(display example-frame)
(newline)
(frame-variables example-frame)
(frame-values example-frame)

;;; testing environments
(define env (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
(lookup-variable-value 'a env)
(define-variable! 'c 5 env)
(display env)
(newline)
(set-variable-value! 'c 10 env)
(display env)
(newline)
(define extended-env (extend-environment (list 'd) (list '20) env))
(display extended-env)
(newline)
(lookup-variable-value 'c extended-env)
(set-variable-value! 'c 30 extended-env)
(display extended-env)
(newline)
(lookup-variable-value 'c extended-env)
(define-variable! 'd 50 extended-env)
(display extended-env)
(newline)
(lookup-variable-value 'd extended-env)