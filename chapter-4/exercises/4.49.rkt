#lang sicp

;;; Exercise 4.49: Alyssa P. Hacker is more interested in
;;; generating interesting sentences than in parsing them.
;;; She reasons that by simply changing the procedure parse-word so
;;; that it ignores the “input sentence” and instead
;;; always succeeds and generates an appropriate word, we can use the
;;; programs we had built for parsing to do generation instead.
;;; Implement Alyssa’s idea, and show the first half-dozen or
;;; so sentences generated.

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (generate-sentence)
  (list 'sentence
        (generate-noun-phrase)
        (generate-verb)))

(define (generate-noun-phrase)
  (list
   'noun-phrase
   (generate-article)
   (generate-noun)))

(define (generate-article)
  (list 'article (an-element-of (cdr articles))))

(define (generate-noun)
  (list 'article (an-element-of (cdr nouns))))

(define (generate-verb)
  (list 'article (an-element-of (cdr verbs))))

(generate-sentence)