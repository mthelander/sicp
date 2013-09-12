(define (p l)
  (pretty-print l))

(define (memq item x)
  (cond
    ((null? x) #f)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

; What would the interpreter print in response to evaluating each of the following expressions?
(list 'a 'b 'c) ; (a b c)

(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f

(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; Exercise 2.54

(define (equal? a b)
  (cond
    ((or (null? a) (null? b))
      (and (null? a) (null? b)))
    ((and (symbol? (car a)) (symbol? (car b)))
      (and (eq? (car a) (car b))
	   (equal? (cdr a) (cdr b))))
    ((or (symbol? (car a))
	 (symbol? (car b))) #f)
    (else (and (equal? (car a) (car b))
	       (equal? (cdr a) (cdr b))))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is a) list) '(this (is a) list))

; Exercise 2.55
(car ''abracadabra)
; This result is quote, because '() is shorthand for (quote ()), so in this case,
; (car ''abracadabra) is the same as (car (quote (quote abracadabra))), which makes it more obvious that the first element is quote.

; Exercise 2.56

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and pair? x) (eq? (car x) '**))

(define (base p) (cadr p))

(define (exponent p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
	(else (list '** e1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	  (if (same-variable? exp var) 1 0))
	((sum? exp)
	  (make-sum (deriv (addend exp) var)
		    (deriv (augend exp) var)))
	((product? exp)
	  (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (deriv (multiplier exp) var)
	 		  (multiplicand exp))))
	((exponentiation? exp)
	  (make-product
	    (make-product (exponent exp) (make-exponentiation
					 (base exp)
					 (- (exponent exp) 1)))
	    (deriv (base exp) var)))
	(else
	  (pretty-print "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)



; Exercise 2.57

(define (count l)
  (cond
   ((null? l) 0)
   (else (+ 1 (count (cdr l))))))

(define (augend s)
  (multiplicand s))

(define (multiplicand p)
  (case (count p)
    ((3) (caddr p))
    (else (cons (car p) (cddr p)))))

(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ x y z) 'x)

; Exercise 2.58a

(define (operator exp) (cadr exp))

(define (sum? x)
  (and (pair? x) (eq? (operator x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (operator x) '*)))

(define (multiplier p) (addend p))

(define (multiplicand p) (augend p))

(define (exponentiation? x)
  (and pair? x) (eq? (operator x) '**))

(define (base p) (addend p))

(define (exponent p) (augend p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
	(else (list e1 '** e2))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

; Exercise 2.58b

; first pass -- wrong!!
;; (define (member? x l)
;;   (cond ((null? l) false)
;; 	((eq? (car l) x) true)
;; 	(else (member? x (cdr l)))))

;; (define (splode-if-single l)
;;   (if (eq? (count l) 1)
;;       (car l) l))

;; (define (left-partition x l)
;;   (if (or (null? l) (eq? (car l) x))
;;       '()
;;       (cons (car l) (left-partition x (cdr l)))))

;; (define (right-partition x l)
;;   (cond ((null? l) '())
;; 	((eq? (car l) x) (cdr l))
;; 	(else (right-partition x (cdr l)))))

;; (define (sum? x)
;;   (and (pair? x) (member? '+ x) (not (product? x))))

;; (define (addend s)
;;   (splode-if-single (left-partition '+ s)))

;; (define (augend s)
;;   (splode-if-single (right-partition '+ s)))

;; (define (product? x)
;;   (and (pair? x) (member? '* x)))

;; (define (multiplier p)
;;   (splode-if-single (left-partition '* p)))

;; (define (multiplicand p)
;;   (splode-if-single (right-partition '* p)))

;; (multiplier '(x + 3 * (x + y + 2)))
;; (multiplicand '(x + 3 * (x + y + 2)))
;; (product? '(x + 3 * (x + y + 2)))
;; (sum? '(x + 3 * (x + y + 2)))
;; (addend '(x + 3))
;; (augend '(x + 3))
;; (trace deriv)



; I feel like this is also wrong, but it calculates the correct answer,
; so we'll go with it.

(define (splode-if-single l)
  (if (eq? (count l) 1)
      (car l) l))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (splode-if-single (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (addend p))

(define (multiplicand p)
  (augend p))

(deriv '(x + 3 * (x + y + 2)) 'x)

; 2.3.3

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Exercise 2.59
; Implement the union-set operation for the unordered-list representation of sets.

; first pass implementation
;; (define (union-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) set2)
;; 	((element-of-set? (car set1) set2)
;; 	 (union-set (cdr set1) set2))
;; 	(else (cons (car set1)
;; 		    (union-set (cdr set1) set2)))))

; slightly more concise implementation
(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      set2 (adjoin-set (car set1) (union-set (cdr set1) set2))))

(union-set '(a b c) '(b c d e))
(union-set '(foo bar) '(baz bak bar))
(union-set '(baz bak bar) '(foo bar))

; Exercise 2.60

(define (adjoin-set x set)
  (cons x set))

(intersection-set '(a b c) '(b c b d a))
(union-set '(a b c) '(b c b d a))
(union-set '(b c b d a) '(a b c))

;; The implementation that allows duplicates is O(n**2), but the one that does allow dups
;; is much faster -- O(n), because it doesn't need to check element membership before
;; inserting a new element. It would be better if our set elements are already guaranteed to be unique.

; Sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(intersection-set '(1 2 3 4 5) '(4 5 6 7))

; Exercise 2.61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((eq? x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 3 '(1 2 3 5 8 13)) ; (1 2 3 5 8 13)
(adjoin-set 13 '(1 2 3 5 8)) ; (1 2 3 5 8 13)
(adjoin-set 1 '(2 3 5 8 13)) ; (1 2 3 5 8 13)
(adjoin-set 5 '(1 2 3 8 13)) ; (1 2 3 5 8 13)

; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2) (union-set (cdr set1) set2))
		 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		 (else (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3 5 8 13) '(5 8 13 21 34)) ; (1 2 3 5 8 13 21 34)
(union-set '(1 2 3 5 8) '(1 2 3 5 8)) ; (1 2 3 5 8)
(union-set '(5 8 13) '(1 2 3 5 8 13 21 34 55)) ; (1 2 3 4 8 13 21 34 55)

; Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; a. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in figure 2.16?
(tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(tree->list-2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
; They all produce the list (1 3 5 7 9 11), so they produce the same lists for all trees I guess...

; b. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?
; They both traverse the entire tree in O(n) time.
; WRONG! I didn't realize that (append) needs to traverse the entire list, so (tree->list-1) is actually O(n**2), making it much less efficient than (tree->list-2).


; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; partial-tree works by partitioning the set of elements into two groups, then generates the partial tree for each partition, assigning the first element of the remaining set to the left branch, and the second to the right branch. Note that it will only work on sorted distinct sets of numerical data.
(partial-tree '(1 3 5 7 9 11) 6)
; 
;     5
;   /   \
;  1     9
;   \   / \
;    3 7   11

; b. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

; Order n growth

; Exercise 2.65

; I misunderstood this problem. Apperently the correct route is to convert to a list and reuse the previous implementations of (union|intersection)-set.
;; (define (intersection-tree tree1 tree2)
;;   (if (or (null? tree1) (null? tree2))
;;       '()
;;       (let ((x1 (entry tree1)) (x2 (entry tree2)))
;; 	(cond ((= x1 x2)
;; 	       (append (intersection-tree (left-branch tree1)
;; 					  (left-branch tree2))
;; 		       (cons x1
;; 			     (intersection-tree (right-branch tree1)
;; 						(right-branch tree2)))))
;;               ((< x1 x2)
;; 	       (append
;; 		(intersection-tree tree1 (left-branch tree2))
;; 		(intersection-tree (right-branch tree1) tree2)))
;; 	      ((> x1 x2)
;; 	       (append
;; 		(intersection-tree (left-branch tree1) tree2)
;; 		(intersection-tree tree1 (right-branch tree2))))))))

(define (intersection-tree tree1 tree2)
  (intersection-set (tree->list-2 tree1) (tree->list-2 tree2)))

(define (union-tree tree1 tree2)
  (union-set (tree->list-2 tree1) (tree->list-2 tree2)))

(intersection-tree (list->tree '(1 2 3 5 8)) (list->tree '(1 2 3 5 8))) ; (1 2 3 5 8)
(intersection-tree (list->tree '(1 2 3 5 8)) (list->tree '(5 8 13 21 34))) ; (5 8)
(intersection-tree (list->tree '(5 8 13 21 24)) (list->tree '(1 2 3 4 5 6 7 8 24))) ; (5 8 24)

(union-tree (list->tree '(1 2 3 5 8)) (list->tree '(1 2 3 5 8))) ; (1 2 3 5 8)
(union-tree (list->tree '(1 2 3 5 8)) (list->tree '(5 8 13 21 34))) ; (1 2 3 5 8 13 21 34)
(union-tree (list->tree '(5 8 13 21 24)) (list->tree '(1 2 3 4 5 6 7 8 24))) ; (1 2 3 4 5 6 7 8 13 21 24)

; Exercise 2.66

(define (key record)
  (if (pair? record) (car record) '()))

(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      '()
      (let ((node (entry tree-of-records))
	    (the-key (key (entry tree-of-records))))
	(cond ((eq? given-key the-key) node)
	      ((> the-key given-key)
	       (lookup given-key
		       (left-branch tree-of-records)))
	      (else (lookup given-key
			    (right-branch tree-of-records)))))))

(lookup '11 '((5 foo) ((1 bar) () ((3 baz) () ())) ((9 bak) ((7 bax) () ()) ((11 bal) () ()))))

; Section 2.3.4

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) ; (a d a b b c a)

; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol) '()))
	((memq symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	((memq symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	(else (error "I don't know what the fark this is ->" symbol))))

(encode-symbol 'a sample-tree) ; ( 0 )
(encode '(a d a b b c a) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (case (length pairs)
    ((0) '())
    ((1) (car pairs))
    (else (successive-merge
	   (adjoin-set
	    (make-code-tree (car pairs) (cadr pairs))
	    (cddr pairs))))))

(define my-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) my-tree) ; (a d a b b c a)

; Exercise 2.70

(define rock-songs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-songs))

(define rock-message (append
		      '(Get a job)
		      '(Sha na na na na na na na na)
		      '(Get a job)
		      '(Sha na na na na na na na na)
		      '(Wah yip yip yip yip yip yip yip yip yip)
		      '(Sha boom)))

(define encoded-rock-message (encode rock-message rock-tree))
(length encoded-rock-message) ; 84
(length rock-message) ; 36
; variable lenth encoding takes 84 bits.
; log2(8) = 3 bits per symbol, so (* 3 36) is 108, so fixed length encoding would take 108 bits.

; Exercise 2.71

; n=5
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))
; ((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16)
;                (A B C D E 31)
;                /            \
;           (A B C D 15)     (E 16)
;           /          \
;       (A B C 7)     (D 8)
;       /       \
;    (A B 3)   (C 4)
;    /     \
;  (A 1)  (B 2)
;
; n=10
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
;((((((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31) (leaf f 32) (a b c d e f) 63) (leaf g 64) (a b c d e f g) 127) (leaf h 128) (a b c d e f g h) 255) (leaf i 256) (a b c d e f g h i) 511) (leaf j 512) (a b c d e f g h i j) 1023)
; (A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)
;
;                                                                      (A B C D E F G H I J 1023)
;                                                                      /                         \
;                                                           (A B C D E F G H I 511)            (J 512)
;                                                           /                     \
;                                                 (A B C D E F G H 255)         (I 256)
;                                                 /                   \
;                                         (A B C D E F G 127)      (H 128)
;                                         /                 \
;                                 (A B C D E F 63)        (G 64)
;                                 /              \
;                          (A B C D E 31)      (F 32)
;                          /            \
;                    (A B C D 15)     (D 16)
;                    /          \
;                (A B C 7)     (D 8)
;                /       \
;            (A B 3)    (C 4)
;            /     \
;          (A 1)  (B 2)
;
; The most frequent symbol requires 1 bit.
; The least frequent symbol requires n-1 bits.


; Excercise 2.72

; O(n^2/2+n), so O(n^2) for the worst case.
