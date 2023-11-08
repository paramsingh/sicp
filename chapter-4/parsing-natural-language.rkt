#lang sicp

(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

;;; grammar: sentence has a noun phrase and a verb
;;; noun phrase is an article followed by a noun
;;; example: (sentence (noun-phrase (article the) (noun cat)) (verb eats))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define unparsed '())
(define (parse-word word-list)
  (require (not (null? unparsed)))
  (require (memq (car unparsed) (cdr word-list)))
  (let ((found-word (car unparsed)))
    (set! unparsed (cdr unparsed))
    (list (car word-list) found-word)))

(define (parse input)
  (set! unparsed input)
  (let ((sent (parse-sentence)))
    (require (null? unparsed))
    sent))

(parse '(the cat in the class eats))