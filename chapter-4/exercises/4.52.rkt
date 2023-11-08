#lang sicp


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