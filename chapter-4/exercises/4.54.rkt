#lang sicp


(define (require? exp)
  (tagged-list? exp 'require?))

(define (require-predicate exp)
  (cadr exp))


(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (suceed 'ok fail2)))
             fail))))