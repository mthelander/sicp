; Section 2.2.1 ------------------------------------------------------------------

(define nil '())

; Exercise 2.17 ------------------------------------------------------------------

(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34)) ; (34)

; Exercise 2.18 ------------------------------------------------------------------

(define (reverse l)
  (if (null? l) '()
      (append (reverse (cdr l))
	      (list (car l)))))

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)

; Exercise 2.19 ------------------------------------------------------------------

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

(cc 100 us-coins) ; 292
(cc 100 (reverse us-coins)) ; 292

; No, the order of coin-values doesn't affect the outcome, because addition is
; commutative.

; Exercise 2.20 ------------------------------------------------------------------

(define (same-parity n . l)
  (let ((proc (if (even? n) even? odd?)))
    (define (iter l)
      (cond ((null? l) '())
	    ((proc (car l))
	     (cons (car l) (iter (cdr l))))
	    (else (iter (cdr l)))))
    (cons n (iter l))))

(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; (2 4 6)

; Exercise 2.21 ------------------------------------------------------------------

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(square-list (list 1 2 3 4)) ; (1 4 9 16)

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4)) ; (1 4 9 16)

; Exercise 2.22 ------------------------------------------------------------------

; Louis Reasoner's version of square-list build the result in reverse order
; because he isn't cons'ing each element onto the next one, each next one is
; cons'ed onto the previous one.

; His second one won't work because cons expects the second argument to be a
; list, and he'll end just up with a deeply nested list as the car of a pair.

; Exercise 2.23 ------------------------------------------------------------------

(define (for-each proc l)
  (cond ((null? l) true)
	(else (proc (car l))
	      (for-each proc (cdr l)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
;; 57
;; 321
;; 88

; Exercise 2.24 ------------------------------------------------------------------

(list 1 (list 2 (list 3 4))) ; (1 (2 (3 4)))

;; Box and pointer diagram:

;; (o|o)--->(o|\)
;;  |        |
;;  v        v
;; (1)      (o|o)--->(o|\)
;;           |        |
;;           v        v
;;          (2)      (o|o)--->(o|\)
;;                    |        |
;;                    v        v
;;                   (3)      (4)

;; Tree:

;;        o (1 (2 (3 4)))
;;       / \
;;      1   o 
;;         / \
;;        2   o
;;           / \
;;          3   4

; Exercise 2.25 ------------------------------------------------------------------

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))) ; 7

(car (car '((7)))) ; 7

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))) ; 7

; Exercise 2.26 ------------------------------------------------------------------

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)

(cons x y) ; ((1 2 3) 4 5 6)

(list x y) ;((1 2 3) (4 5 6))

; Exercise 2.27 ------------------------------------------------------------------

(define (deep-reverse l)
  (cond ((null? l) '())
	((pair? (car l))
	 (append (deep-reverse (cdr l))
		 (list (deep-reverse (car l)))))
	(else (append (deep-reverse (cdr l))
		      (list (car l))))))

(define x (list (list 1 2) (list 3 4)))

(reverse x) ; ((3 4) (1 2))

(deep-reverse x) ; ((4 3) (2 1))

; Exercise 2.28 ------------------------------------------------------------------

(define (fringe tree)
  (cond ((null? tree) '())
	((pair? (car tree))
	 (append (fringe (car tree))
		 (fringe (cdr tree))))
	(else (cons (car tree)
		    (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)

; Exercise 2.29 ------------------------------------------------------------------

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

; b.

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (branch-weight b)
  (let ((struct (branch-structure b)))
    (if (pair? struct)
	(total-weight struct)
	struct)))

(define m1 (make-mobile (make-branch 2 3) (make-branch 2 7)))
(total-weight m1) ; 10

(define m2 (make-mobile (make-branch 3 9)
			(make-branch 1 (make-mobile (make-branch 3 7)
						    (make-branch 2 5)))))

(total-weight m2) ; 21

; c.

(define (torque b)
  (* (branch-weight b)
     (branch-length b)))

(define (branch-balanced? b)
  (let ((struct (branch-structure b)))
    (if (pair? struct)
	(balanced? struct)
	true)))

(define (balanced? m)
  (let ((left (left-branch m))
	(right (right-branch m)))
    (and (branch-balanced? left)
	 (branch-balanced? right)
	 (= (torque left)
	    (torque right)))))

(balanced? m1) ; false
(balanced? (make-mobile (make-branch 2 16)
			(make-branch 2 16))) ; true

; d.

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; This much:

(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

(balanced? (make-mobile (make-branch 2 16)
			(make-branch 2 16))) ; true

; Exercise 2.30 ------------------------------------------------------------------

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree))
	 (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (* subtree subtree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.31 ------------------------------------------------------------------

(define (tree-map proc tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (proc subtree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

; Exercise 2.32 ------------------------------------------------------------------

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))

; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This works because we can recursively define the subsets of s to be the subsets
; of the rest of the elements of s, and those subsets with the first element
; consed onto them.

; Section 2.2.1 ------------------------------------------------------------------

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

; Exercise 2.33 ------------------------------------------------------------------

;; I'm leaving these commented out, since they override the built-in functions.

;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))

;; (define (length sequence)
;;   (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; (map square '(1 2 3 4 5)) ; (1 4 9 16 25)
;; (append '(4 5 6) '(8 9 10)) ; (4 5 6 8 9 10)
;; (length '(6 7 8 9 10 11))

; Exercise 2.34 ------------------------------------------------------------------

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79

; Exercise 2.35 ------------------------------------------------------------------

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (cons (list 1 2) (list 3 4))) ; 4

; Exercise 2.36 ------------------------------------------------------------------

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; (22 26 30)

; Exercise 2.37 ------------------------------------------------------------------

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(dot-product '(2 3 4) '(5 6 7)) ; 56
(matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(10 11 12)) ; (68 167 266)
(transpose '((1 2 3) (4 5 6) (7 8 9))) ; ((1 4 7) (2 5 8) (3 6 9))
(matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))
; ((30 36 42) (66 81 96) (102 126 150))

; Exercise 2.38 ------------------------------------------------------------------

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

; op should define a commutative operation, such as +, in order to guarantee that
; both with return the same value for any sequence.

; Exercise 2.39 ------------------------------------------------------------------

(define (reverse sequence)
  (fold-right (lambda (x a) (append a (list x))) nil sequence))

(reverse '(1 2 3 4)) ; (4 3 2 1)

(define (reverse sequence)
  (fold-left (lambda (x a) (cons a x)) nil sequence))

(reverse '(1 2 3 4)) ; (4 3 2 1)

; Exercise 2.40 ------------------------------------------------------------------

; Setup code

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; From 1.2

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

; end 1.2

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; end setup code

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 4) ; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))
(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; Exercise 2.41 ------------------------------------------------------------------

(define (triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
		(map (lambda (z) (list i j z))
		     (enumerate-interval 1 (- j 1))))
	      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triples-sum n s)
  (filter (lambda (p) (= (accumulate + 0 p) s))
	  (triples n)))

(triples 4) ; ((3 2 1) (4 2 1) (4 3 1) (4 3 2))
(triples-sum 4 6) ; ((3 2 1))

; Exercise 2.42 ------------------------------------------------------------------

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())
(define (adjoin-position row k queens)
  (cons (list row k) queens))
(define (slope p1 p2)
  (let ((numer (- (cadr p1) (cadr p2)))
	(denom (- (car p1) (car p2))))
    (if (or (= 0 numer) (= 0 denom)) 0
	(/ numer denom))))
(define (is-safe? k-queen other)
  (let ((m (abs (slope k-queen other))))
    (not (or (= m 0) (= m 1)))))

(define (safe? k positions)
  (let ((k-queen (car positions)))
    (define (iter-rest rest-queens)
      (if (null? rest-queens) true
	  (and (is-safe? k-queen (car rest-queens))
	       (iter-rest (cdr rest-queens)))))
    (iter-rest (cdr positions))))

(car (queens 8)) ; ((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1))

; Exercise 2.43 ------------------------------------------------------------------

; Louis Reasoner's mixup causes the program to recurse for every step of the
; innermost map, rather than just k times. If the original program completes in T
; time, then I would estimate this one to complete in T^k times, since each k
; step spawns a whole new series of k calculations.

; Exercise 2.44 ------------------------------------------------------------------

(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; Exercise 2.45 ------------------------------------------------------------------

(define (split x y)
  (lambda (painter n)
    (if (= n 0) painter
	(let ((smaller ((split x y) painter (- n 1))))
	  (x painter (y smaller smaller))))))

; Exercise 2.46 ------------------------------------------------------------------

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect v2 -1)))
(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

(define a (make-vect 3 5))
(define b (make-vect 2 6))

(add-vect a b) ; (5 . 11)
(sub-vect a b) ; (1 . -1)
(scale-vect a 10) ; (30 . 50)

; Exercise 2.47 ------------------------------------------------------------------

; First method:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define f (make-frame (make-vect 0 0) (make-vect 2 2) (make-vect 1 1)))
(origin-frame f) ; (0 . 0)
(edge1-frame f) ; (2 . 2)
(edge2-frame f) ; (1 . 1)

; Second method:

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge2-frame f) (cddr f))

(define f (make-frame (make-vect 0 0) (make-vect 2 2) (make-vect 1 1)))
(origin-frame f) ; (0 . 0)
(edge1-frame f) ; (2 . 2)
(edge2-frame f) ; (1 . 1)

; Exercise 2.48 ------------------------------------------------------------------

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define seg-test (make-segment (make-vect 3 6)
			       (make-vect 5 7)))

(start-segment seg-test) ; (3 . 6)
(end-segment seg-test) ; (5 . 7)

; Exercise 2.49 ------------------------------------------------------------------

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define upper-left (make-vect 0 1))
(define upper-right (make-vect 1 1))
(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))

; a.

(define frame-outline-painter (segments->painter
			       (list (make-segment bottom-left upper-left)
				     (make-segment upper-left upper-right)
				     (make-segment upper-right bottom-right)
				     (make-segment bottom-right bottom-left))))

; b.

(define x-painter (segments->painter
		   (list (make-segment bottom-left upper-right)
			 (make-segment bottom-right upper-left))))

; c.

(define diamond-painter (segments->painter
			 (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
			       (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
			       (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
			       (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))

; d.

; Yeeeeeaaaahhhh... No. This exercise is completedly pointless, and I can't
; even test it!

; Exercise 2.50 ------------------------------------------------------------------

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0) ; new origin
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

; Exercise 2.51 ------------------------------------------------------------------

; a.

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              split-point
			      (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-down
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
			      split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

; b.

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
		    (rotate270 painter2))))

; Exercise 2.52 ------------------------------------------------------------------

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; a.

(define altered-wave
  (lambda (frame)
    ; Just guestimating for the coordinates
    ((frame-coord-map frame) (make-vect 1/3 1/6))
    ((frame-coord-map frame) (make-vect (+ 1/3 1/10) 1/6))
    (wave frame)))

; b.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
	    (bottom-right right (right-split painter (- n 1)))
	    (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner)))))

; c.

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
