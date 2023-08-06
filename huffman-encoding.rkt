#lang racket


(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (equal? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree) (if (leaf? tree)
                           (list (symbol-leaf tree))
                           (caddr tree)))
(symbols (make-leaf 'a 10))

(define (make-code-tree left right)
  (println (symbols left))
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (weight tree) (if (leaf? tree)
                          (weight-leaf tree)
                          (cadddr tree)))

(define (decode bits tree)
  (define (decode-helper bits node)
    (cond
      ((leaf? node) (append (list (symbol-leaf node)) (decode bits tree)))
      ((null? bits) '())
      ((= (car bits) 0) (decode-helper (cdr bits) (left-branch node)))
      ((= (car bits) 1) (decode-helper (cdr bits) (right-branch node)))))
  (decode-helper bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((> (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (adjoin-set (make-leaf (car (car pairs)) (cdr (car pairs)))
                  (make-leaf-set (cdr pairs)))))


;;; Exercise 2.67: Define an encoding tree and a sample message:
(println "probably getting here")
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; 0 -> A
;; 110 -> D
;; 0 -> A
;; 10 -> B
;; 10 -> B
;; 111 -> C
;; 0 -> A
;; ADABBCA
;;; Use the decode procedure to decode the message, and give
;;; the result.

(println "trying to decode")
(println sample-tree)
(decode sample-message sample-tree) ;; '(A B B A)


;;; Exercise 2.68: e encode procedure takes as arguments a
;;; message and a tree and produces the list of bits that gives
;;; the encoded message.

;;; encode-symbol is a procedure, which you must write, that
;;; returns the list of bits that encodes a given symbol according to a given tree. You should design encode-symbol so
;;; that it signals an error if the symbol is not in the tree at all.
;;; Test your procedure by encoding the result you obtained in
;;; Exercise 2.67 with the sample tree and seeing whether it is
;;; the same as the original sample message.

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (equal? (symbol-leaf tree) symbol)
             '()
             (error "symbol not in tree")))
        ((member symbol (symbols (left-branch tree))) (append (list 0) (encode-symbol symbol (left-branch tree))))
        ((member symbol (symbols (right-branch tree))) (append (list 1) (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree"))))

(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode '(A B B A) sample-tree)
(decode (encode '(A D A B B C A) sample-tree) sample-tree)

;;; Exercise 2.69: The following procedure takes as its argument a list of
;;; symbol-frequency pairs (where no symbol
;;; appears in more than one pair) and generates a Huffman
;;; encoding tree according to the Huffman algorithm.

;;; (define (generate-huffman-tree pairs)
;;;   (successive-merge (make-leaf-set pairs)))

;;; make-leaf-set is the procedure given above that transforms the list of pairs
;;; into an ordered set of leaves. successive-merge is the procedure you must write,
;;; using make-codetree to successively merge the smallest-weight elements
;;; of the set until there is only one element left, which is the
;;; desired Huffman tree. (This procedure is slightly tricky, but
;;; not really complicated. If you find yourself designing a complex procedure,
;; then you are almost certainly doing something wrong. You can take significant advantage of the fact
;;; that we are using an ordered set representation.)

(define (successive-merge leaves)
  (cond ((= (length leaves) 1) (car leaves))
        (else (make-code-tree
               (car leaves)
               (successive-merge (cdr leaves))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;;; The following eight-symbol alphabet with
;;; associated relative frequencies was designed to efficiently
;;; encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)
;;; A 2 GET 2 SHA 3 WAH 1
;;; BOOM 1 JOB 2 NA 16 YIP 9

(define alphabet-pairs
  (list
   (cons 'A 2)
   (cons 'GET 2)
   (cons 'SHA 3)
   (cons 'WAH 1)
   (cons 'BOOM 1)
   (cons 'JOB 2)
   (cons 'NA 16)
   (cons 'YIP 9)))

(print "here are the pairs")
(print alphabet-pairs)

(define alphabet-set (make-leaf-set alphabet-pairs))
(print "set generated")
(print alphabet-set)

(define rock-tree (generate-huffman-tree alphabet-pairs))
(println rock-tree)

;;; (print "tree generated")

;;; Use generate-huffman-tree (Exercise 2.69) to generate a
;;; corresponding Huffman tree, and use encode (Exercise 2.68)
;;; to encode the following message:

;;; Get a job
;;; Sha na na na na na na na na
;;; Get a job
;;; Sha na na na na na na na na
;;; Wah yip yip yip yip yip yip yip yip yip
;;; Sha boom

(define message
  (list
   'GET 'A 'JOB
   'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
   'GET 'A 'JOB
   'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
   'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
   'SHA 'BOOM
   ))

(print (length (encode message rock-tree)))
(decode (encode message rock-tree) rock-tree)