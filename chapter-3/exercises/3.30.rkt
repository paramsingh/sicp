#lang sicp

;;; Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed
;;; by stringing togethern full-adders.This is the simplest form
;;; of parallel adder for adding two n-bit binary numbers. The
;;; inputs A1, A2, A3, . . ., An and B1, B2, B3, . . ., Bn are the
;;; two binary numbers to be added (each Ak and Bk is a 0 or
;;; a 1). The circuit generates S1, S2, S3, . . ., Sn, the n bits of
;;; the sum, and C, the carry from the addition. Write a procedure
;;; ripple-carry-adder that generates this circuit. The
;;; procedure should take as arguments three lists of n wires
;;; each — the Ak , the Bk , and the Sk — and also another wire C.
;;; The major drawback of the ripple-carry adder is the need
;;; to wait for the carry signals to propagate. What is the delay
;;; needed to obtain the complete output from an n-bit ripple-carry adder, expressed in terms of the delays for and-gates,
;;; or-gates, and inverters?


(define (ripple-carry-adder ak bk sk c)
  (define (iter ak bk sk c)
    (if (null? ak)
        'done
        (begin
          (full-adder (car ak) (car bk) (car sk) c)
          (iter (cdr ak) (cdr bk) (cdr sk) c))))
  (iter ak bk sk c))