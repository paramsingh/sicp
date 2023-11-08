#lang sicp

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