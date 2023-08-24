#lang sicp

;;; (define random-init 42)

;;; (define (rand-update x) (+ x 1))

;;; (define rand
;;;   (let ((x random-init))
;;;     (lambda ()
;;;       (set! x (rand-update x))
;;;       x)))


;;; (define (estimate-pi trials)
;;;   (sqrt ( / (monte-carlo cesaro trials) 6)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; (define (cesaro)
;;;   (= (gcd (random 100) (random 1000)) 1))

(define (monte-carlo test trials)
  (define (iter pass rem)
    (cond ((= rem 0) (/ pass trials))
          ((test) (iter (+ pass 1) (- rem 1)))
          (else (iter pass (- rem 1)))))
  (iter 0 trials))


(random 1000)