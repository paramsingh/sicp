#lang sicp

(define (get operation division) '())

(define (get-record division employee)
  ((get 'record division) employee))


(define (get-salary division employee)
  ((get 'salary division) employee))

(define (find-employee-record employee divisions)
  (if (null? divisions)
      '()
      (or (get-record employee (car divisions))
          (find-employee-record employee (cdr divisions)))))