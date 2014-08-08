; Section 4.3.1 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch4-ambeval.scm")

(define the-global-environment (setup-environment))

(define (run exp)
  (ambeval exp the-global-environment
	   (lambda (val next) val)
	   (lambda () (display "fail"))))

(define-syntax ->
  (er-macro-transformer
   (lambda (form rename cmp)
     (let ((exp (cadr form)))
       `(,(rename 'run) (quote ,exp))))))

(-> (define (require p)
      (if (not p) (amb))))

(-> (define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1)))))

; Exercise 4.35 ------------------------------------------------------------------

(-> (define (an-integer-between low high)
      (require (<= low high))
      (amb low (an-integer-between (+ low 1) high))))

(-> (define (a-pythagorean-triple-between low high)
      (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between i high)))
	  (let ((k (an-integer-between j high)))
	    (require (= (+ (* i i) (* j j)) (* k k)))
	    (list i j k))))))

;(-> (a-pythagorean-triple-between 1 10)) ; (3 4 5)

; Exercise 4.36 ------------------------------------------------------------------

; Because there's no upper bound on the number of choices for each of the three
; amb expressions it would just continue to increase the innermost choice towards
; infinity, never reaching a solution.

; The correct way to solve this is to only make the k-term unbounded, and i and j
; will be restricted to the finite list of numbers less than k.

(-> (define (pythagorean-triples)
      (let ((k (an-integer-starting-from 1)))
	(let ((i (an-integer-between 1 k)))
	  (let ((j (an-integer-between i k)))
	    (require (= (+ (* i i) (* j j)) (* k k)))
	    (list i j k))))))

;(-> (pythagorean-triples)) ; (3 4 5)

; Exercise 4.37 ------------------------------------------------------------------

; Yes! Because he's eliminated one of the amb expressions, so the number of
; possible paths is reduced from a*b*c to a*b.

; Section 4.3.2 ------------------------------------------------------------------

(-> (define (distinct? items)
      (cond ((null? items) true)
	    ((null? (cdr items)) true)
	    ((member (car items) (cdr items)) false)
	    (else (distinct? (cdr items))))))

(-> (define (multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
	    (cooper (amb 1 2 3 4 5))
	    (fletcher (amb 1 2 3 4 5))
	    (miller (amb 1 2 3 4 5))
	    (smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith)))))

;(-> (multiple-dwelling)) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

; Exercise 4.38 ------------------------------------------------------------------

(define (run exp)
  (ambeval exp the-global-environment
	   (lambda (val next) (display val) (newline) (next))
	   (lambda () (display "fail"))))

(-> (define (multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
	    (cooper (amb 1 2 3 4 5))
	    (fletcher (amb 1 2 3 4 5))
	    (miller (amb 1 2 3 4 5))
	    (smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith)))))

;; (-> (multiple-dwelling))
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
;; fail

; There are 5 solutions to this version.

; Exercise 4.39 ------------------------------------------------------------------

; No, it does not affect the answer, but it will affect the order in which they
; are generated, and the time in which some of them are found. For instance,
; having the adjacency requirement at the end causes the interpreter to get all
; the way through the rest of the requires but must backtrack and recheck them
; all.

; Exercise 4.40 ------------------------------------------------------------------

; Before the distinct requirement there are 5^5=3125 possibilities, which is
; reduced down to 5!=120 after the distinct requirement.

(-> (define (multiple-dwelling)
      (define (range-with-non-adjacent-elements r n)
	(require (not (null? r)))
	(if (member (car r) (list (- n 1) n (+ n 1)))
	    (range-with-non-adjacent-elements (cdr r) n)
	    (amb (car r)
		 (range-with-non-adjacent-elements (cdr r) n))))
      (let ((baker (amb 1 2 3 4)))
	(let ((cooper (amb 2 3 4)))
	  (let ((fletcher (range-with-non-adjacent-elements (list 2 3 4) cooper)))
	    (let ((miller (an-integer-between cooper 5)))
	      (let ((smith (range-with-non-adjacent-elements (list 1 2 3 4 5) fletcher)))
		(require (distinct? (list baker cooper fletcher miller smith)))
		(list (list 'baker baker)
		      (list 'cooper cooper)
		      (list 'fletcher fletcher)
		      (list 'miller miller)
		      (list 'smith smith)))))))))

;(-> (multiple-dwelling)) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

; Exercise 4.41 ------------------------------------------------------------------

(define (multiple-dwelling)
  (define (distinct? items)
    (cond ((null? items) true)
	  ((null? (cdr items)) true)
	  ((member (car items) (cdr items)) false)
	  (else (distinct? (cdr items)))))

  (define (nth-element list n)
    (if (zero? n)
	(car list)
	(nth-element (cdr list) (- n 1))))

  (define (solves-the-problem? list)
    (let ((b (nth-element list 0))
	  (c (nth-element list 1))
	  (f (nth-element list 2))
	  (m (nth-element list 3))
	  (s (nth-element list 4)))
      (and (distinct? list)
	   (not (= b 5))
	   (not (= c 1))
	   (not (= f 5))
	   (not (= f 1))
	   (> m c)
	   (not (= (abs (- s f)) 1))
	   (not (= (abs (- f c)) 1)))))

  (define (product b c f m s)
    (append-map (lambda (b)
  	   (append-map (lambda (c)
  		  (append-map (lambda (f)
  			 (append-map (lambda (m)
  				(map (lambda (s) (list b c f m s)) s)) m)) f)) c)) b))

    (filter solves-the-problem? (product
				 (list 1 2 3 4 5)
				 (list 1 2 3 4 5)
				 (list 1 2 3 4 5)
				 (list 1 2 3 4 5)
				 (list 1 2 3 4 5))))

(multiple-dwelling) ; ((3 2 4 5 1))

; Exercise 4.42 ------------------------------------------------------------------

(-> (define (liars)
      (define (distinct? items)
	(cond ((null? items) true)
	      ((null? (cdr items)) true)
	      ((member (car items) (cdr items)) false)
	      (else (distinct? (cdr items)))))
      (let ((betty (amb 1 2 3))
	    (ethel (amb 1 5))
	    (joan  (amb 2 3))
	    (kitty 2)
	    (mary 4))
	(require (distinct? (list betty ethel joan kitty mary)))
	(list
	 (list 'betty betty)
	 (list 'ethel ethel)
	 (list 'joan joan)
	 (list 'kitty kitty)
	 (list 'mary mary)))))

(-> (liars)) ; ((betty 1) (ethel 5) (joan 3) (kitty 2) (mary 4))

; Exercise 4.43 ------------------------------------------------------------------

(-> (define (yachts)
      (define (yacht exp) (car exp))
      (define (daughter exp) (car (cdr exp)))
      (define (find pred list)
      	(cond ((null? list) '())
      	      ((pred (car list)) (car list))
      	      (else (find pred (cdr list)))))
      (define (map proc list)
	(cond ((null? list) '())
	      (else (cons (proc (car list))
			  (map proc (cdr list))))))
      (define (distinct? items)
	(cond ((null? items) true)
	      ((null? (cdr items)) true)
	      ((member (car items) (cdr items)) false)
	      (else (distinct? (cdr items)))))
      (let ((mr-moore        (list 'lorna 'mary-ann-moore))
	    (sir-barnacle    (list 'gabrielle 'melissa))
	    (colonel-downing (list 'melissa  (amb 'gabrielle 'rosalind 'lorna)))
	    (mr-hall         (list 'rosalind (amb 'gabrielle 'lorna)))
	    (dr-parker       (list 'mary-ann (amb 'gabrielle 'rosalind 'lorna))))
	(let ((yacht-owners (list mr-moore sir-barnacle colonel-downing
				  mr-hall dr-parker)))
	  (require
	   (distinct? (map daughter yacht-owners)))
	  (let ((gabrielles-father (find (lambda (x) (eq? (daughter x) 'gabrielle))
					 yacht-owners)))
	    (require (eq? (yacht gabrielles-father)
			  (daughter dr-parker))))
	  (list (list 'mr-moore mr-moore)
		(list 'sir-barnacle sir-barnacle)
		(list 'colonel-downing colonel-downing)
		(list 'mr-hall mr-hall)
		(list 'dr-parker dr-parker))))))

(-> (yachts))

;; ((mr-moore (lorna mary-ann-moore))
;;  (sir-barnacle (gabrielle melissa))
;;  (colonel-downing (melissa lorna)) ; <-- The answer is Colonel Downing
;;  (mr-hall (rosalind gabrielle))
;;  (dr-parker (mary-ann rosalind)))

(-> (define (yachts-without-knowing-mary-anns-last-name)
      (define (yacht exp) (car exp))
      (define (daughter exp) (car (cdr exp)))
      (define (find pred list)
      	(cond ((null? list) '())
      	      ((pred (car list)) (car list))
      	      (else (find pred (cdr list)))))
      (define (map proc list)
	(cond ((null? list) '())
	      (else (cons (proc (car list))
			  (map proc (cdr list))))))
      (define (distinct? items)
	(cond ((null? items) true)
	      ((null? (cdr items)) true)
	      ((member (car items) (cdr items)) false)
	      (else (distinct? (cdr items)))))
      (let ((mr-moore        (list 'lorna (amb 'gabrielle 'rosalind 'mary-ann)))
	    (sir-barnacle    (list 'gabrielle 'melissa))
	    (colonel-downing (list 'melissa  (amb 'gabrielle 'rosalind 'lorna 'mary-ann)))
	    (mr-hall         (list 'rosalind (amb 'gabrielle 'lorna 'mary-ann)))
	    (dr-parker       (list 'mary-ann (amb 'gabrielle 'rosalind 'lorna))))
	(let ((yacht-owners (list mr-moore sir-barnacle colonel-downing
				  mr-hall dr-parker)))
	  (require
	   (distinct? (map daughter yacht-owners)))
	  (let ((gabrielles-father (find (lambda (x) (eq? (daughter x) 'gabrielle))
					 yacht-owners)))
	    (require (eq? (yacht gabrielles-father)
			  (daughter dr-parker))))
	  (list (list 'mr-moore mr-moore)
		(list 'sir-barnacle sir-barnacle)
		(list 'colonel-downing colonel-downing)
		(list 'mr-hall mr-hall)
		(list 'dr-parker dr-parker))))))

(-> (yachts-without-knowing-mary-anns-last-name))
;; ((mr-moore (lorna gabrielle)) (sir-barnacle (gabrielle melissa)) (colonel-downing (melissa rosalind)) (mr-hall (rosalind mary-ann)) (dr-parker (mary-ann lorna)))
;; ((mr-moore (lorna mary-ann)) (sir-barnacle (gabrielle melissa)) (colonel-downing (melissa lorna)) (mr-hall (rosalind gabrielle)) (dr-parker (mary-ann rosalind)))

; There are two solutions to the last question; Colonel Downing and Dr. Parker.

; Exercise 4.44 ------------------------------------------------------------------

(-> (define (queens num)
      (define (cadr exp) (car (cdr exp)))
      (define (abs n) (if (< n 0) (* -1 n) n))
      (define (on-diagonal? qn1 qn2)
      	(= (abs (- (car qn1) (cadr qn1)))
	   (abs (- (car qn2) (cadr qn2)))))
      (define (on-horizontal? qn1 qn2)
	(= (car qn1) (car qn2)))
      (define (on-vertical? qn1 qn2)
	(= (cadr qn1) (cadr qn2)))
      (define (and exp)
	(cond ((null? (cdr exp)) (not (not (car exp))))
	      ((car exp) (and (cdr exp)))
	      (else false)))
      (define (map proc list)
	(if (null? list) '()
	    (cons (proc (car list))
		  (map proc (cdr list)))))
      (define (range min max)
	(if (> min max) '()
	    (cons min (range (+ min 1) max))))
      (define (list-diff list exclude)
	(cond ((null? list) '())
	      ((member (car list) exclude)
	       (list-diff (cdr list) exclude))
	      (else (cons (car list)
			  (list-diff (cdr list) exclude)))))
      (define (safe? queen other-queens)
      	(if (null? other-queens) true
      	    (and (list (not (on-diagonal? queen (car other-queens)))
		       (not (on-horizontal? queen (car other-queens)))
		       (not (on-vertical? queen (car other-queens)))
		       (safe? queen (cdr other-queens))))))
      (define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))
      (define (except list)
	(an-element-of (list-diff (range 1 8) list)))
      (define (queen-n n positions)
	;(pp (list-diff (range 1 8) (map cadr positions)))
	(if (> n num)
	    positions
	    (let ((qn (list n (except (map cadr positions)))))
	      (require (safe? qn positions))
	      (queen-n (+ n 1) (cons qn positions)))))
      (queen-n 1 '())))

;(-> (queens 8)) ; Takes a loooong time to complete, but it works!

; Exercise 4.45 ------------------------------------------------------------------

(-> (define nouns '(noun student professor cat class)))
(-> (define verbs '(verb studies lectures eats sleeps)))
(-> (define articles '(article the a)))
(-> (define prepositions '(prep for to in by with)))

(-> (define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (memq (car *unparsed*) (cdr word-list)))
      (let ((found-word (car *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) found-word))))

(-> (define *unparsed* '()))
(-> (define (parse input)
      (set! *unparsed* input)
      (let ((sent (parse-sentence)))
	(require (null? *unparsed*))
	sent)))

(-> (define (parse-prepositional-phrase)
      (list 'prep-phrase
	    (parse-word prepositions)
	    (parse-noun-phrase))))

(-> (define (parse-sentence)
      (list 'sentence
	    (parse-noun-phrase)
	    (parse-verb-phrase))))
(-> (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
	(amb verb-phrase
	     (maybe-extend (list 'verb-phrase
				 verb-phrase
				 (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs))))

(-> (define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
	    (parse-word articles)
	    (parse-word nouns))))
(-> (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
	(amb noun-phrase
	     (maybe-extend (list 'noun-phrase
				 noun-phrase
				 (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase))))

;; ;;; Amb-Eval input:
;; (parse '(the professor lectures to the student in the class with the cat))

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor))
;; 	  (verb-phrase (verb-phrase (verb-phrase (verb lectures)
;; 						 (prep-phrase (prep to)
;; 							      (simple-noun-phrase (article the) (noun student))))
;; 				    (prep-phrase (prep in)
;; 						 (simple-noun-phrase (article the) (noun class))))
;; 		       (prep-phrase (prep with)
;; 				    (simple-noun-phrase (article the)
;; 							(noun cat)))))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor))
;; 	  (verb-phrase (verb-phrase (verb lectures)
;; 				    (prep-phrase (prep to)
;; 						 (simple-noun-phrase (article the)
;; 								     (noun student))))
;; 		       (prep-phrase (prep in)
;; 				    (noun-phrase (simple-noun-phrase (article the)
;; 								     (noun class))
;; 						 (prep-phrase (prep with)
;; 							      (simple-noun-phrase (article the)
;; 										  (noun cat)))))))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor))
;; 	  (verb-phrase (verb-phrase (verb lectures)
;; 				    (prep-phrase (prep to)
;; 						 (noun-phrase (simple-noun-phrase (article the)
;; 										  (noun student))
;; 							      (prep-phrase (prep in)
;; 									   (simple-noun-phrase (article the)
;; 											       (noun class))))))
;; 		       (prep-phrase (prep with)
;; 				    (simple-noun-phrase (article the)
;; 							(noun cat)))))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the)
;; 			      (noun professor))
;; 	  (verb-phrase (verb lectures)
;; 		       (prep-phrase (prep to)
;; 				    (noun-phrase (noun-phrase (simple-noun-phrase (article the)
;; 										  (noun student))
;; 							      (prep-phrase (prep in)
;; 									   (simple-noun-phrase (article the)
;; 											       (noun class))))
;; 						 (prep-phrase (prep with)
;; 							      (simple-noun-phrase (article the)
;; 										  (noun cat)))))))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor))
;; 	  (verb-phrase (verb lectures)
;; 		       (prep-phrase (prep to)
;; 				    (noun-phrase (simple-noun-phrase (article the)
;; 								     (noun student))
;; 						 (prep-phrase (prep in)
;; 							      (noun-phrase (simple-noun-phrase (article the)
;; 											       (noun class))
;; 									   (prep-phrase (prep with)
;; 											(simple-noun-phrase (article the) (noun cat)))))))))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (parse (quote (the professor lectures to the student in the class with the cat)))

; With the cat, the professor lectures to the student, the student is in the class
; With the cat, the professor lectures to the student, the professor is in the class
; The professor lectures to the student, the student is in the class, the student is with the cat
; The professor lectures to the student, the student is in the class with the cat.
; The professor lectures to the student, the professor is in the class with the cat

; Exercise 4.46 ------------------------------------------------------------------

; Because english sentences are meant to be evaluated left-to-right and make no
; sense otherwise. Because our evaluator maintains a global state that is
; manipulated throughout the parsing it is important that the left-to-right
; parsing be preserved.

; Exercise 4.47 ------------------------------------------------------------------

; No, Louis Reasoner is wrong, because each call to parse-word is destructive and
; therefore at each recursive iteration his procedure consumes another verb, which
; will cause an infinite loop. This outcome is the same if we interchange the order
; of the amb expressions.

;; (-> (parse '(the student with the cat sleeps in the class))) ; Gives output

;; (-> (define (parse-verb-phrase)
;;       (amb (parse-word verbs)
;; 	   (list 'verb-phrase
;; 		 (parse-verb-phrase)
;; 		 (parse-prepositional-phrase)))))

;; (-> (parse '(the student with the cat sleeps in the class))) ; Never returns

; Exercise 4.48 ------------------------------------------------------------------

(-> (define adjectives '(adj cold blue shiny hirsute)))
(-> (define adverbs '(adv foolishly often rarely)))

(-> (define (parse-simple-noun-phrase)
      (amb (list 'simple-noun-phrase
		 (parse-word articles)
		 (parse-word adjectives)
		 (parse-word nouns))
	   (list 'simple-noun-phrase
		 (parse-word articles)
		 (parse-word nouns)))))

(-> (define (parse-sentence)
      (list 'sentence
	    (parse-noun-phrase)
	    (parse-verb-phrase)
	    (parse-adverb-phrase))))
(-> (define (parse-adverb-phrase)
      (list 'adv (parse-word adverbs))))

(-> (parse '(the hirsute cat eats)))
; (sentence (simple-noun-phrase (article the) (adj hirsute) (noun cat)) (verb eats))

(-> (parse '(the hirsute cat sleeps by the student)))
;; (sentence (simple-noun-phrase (article the)
;; 			      (adj hirsute)
;; 			      (noun cat))
;; 	  (verb-phrase (verb sleeps)
;; 		       (prep-phrase (prep by)
;; 				    (simple-noun-phrase (article the)
;; 							(noun student)))))

(-> (parse '(the hirsute cat sleeps by the student often)))
;; (sentence (simple-noun-phrase (article the)
;; 			      (adj hirsute)
;; 			      (noun cat))
;; 	  (verb-phrase (verb sleeps)
;; 		       (prep-phrase (prep by)
;; 				    (simple-noun-phrase (article the)
;; 							(noun student))))
;; 	  (adv (adv often)))

; Exercise 4.49 ------------------------------------------------------------------

(-> (define (parse-word word-list)
      (define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))
      (require (not (null? *unparsed*)))
      (set! *unparsed* (cdr *unparsed*))
      (an-element-of (cdr word-list))))

;(-> (parse '(the hirsute cat sleeps)))
;; the student studies foolishly
;; the student studies often
;; the student studies rarely
;; the student lectures foolishly
;; ...

; Exercise 4.50 ------------------------------------------------------------------

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (define (element-at n list)
    (cond ((null? list) '())
	  ((= 0 n) (car list))
	  (else (element-at (- n 1) (cdr list)))))
  (define (delete-at n list)
    (cond ((null? list) '())
	  ((= n 0) (cdr list))
	  (else (cons (car list)
		      (delete-at (- n 1) (cdr list))))))
  (define (shuffle list)
    (if (null? list) '()
      (let ((r (random (length list))))
	(cons (element-at r list)
	      (shuffle (delete-at r list))))))
  (let ((cprocs (shuffle (map analyze (ramb-choices exp)))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define prev-analyze analyze)
(define (analyze exp)
  (if (ramb? exp)
      (analyze-ramb exp)
      (prev-analyze exp)))

(-> (let ((x (ramb 4 5 6 7 8 9)))
      (require (> x 5))
      x))

;; NOTE: This only works for simple expressions. More complicated ramb exps
;; won't really be selected in random order, because we'd need to evaluate
;; the entire expression beforehand. The following doesn't work correctly:

;; (-> (define (a-random-integer-between low high)
;;       (require (<= low high))
;;       (ramb low (a-random-integer-between (+ low 1) high))))

;; (-> (a-random-integer-between 4 7)) ; NOT IN RANDOM ORDER


; Exercise 4.51 ------------------------------------------------------------------

(define prev-analyze2 analyze)
(define (analyze exp)
  (if (permanent-assignment? exp)
      (analyze-permanent-assignment exp)
      (prev-analyze2 exp)))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok fail2)))
             fail))))

;; (define count 0)
;; (let ((x (an-element-of '(a b c)))
;;       (y (an-element-of '(a b c))))
;;   (set! count (+ count 1))
;;   (require (not (eq? x y)))
;;   (list x y count))
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (a b 1)
;; ;;; Amb-Eval input:
;; try-again
;; ;;; Amb-Eval value:
;; (a c 1)

; They're the same results, except count is always 1, because it is always
; reset when it backtracks.

; Exercise 4.52 ------------------------------------------------------------------

(define prev-analyze3 analyze)
(define (analyze exp)
  (if (if-fail? exp)
      (analyze-if-fail exp)
      (prev-analyze3 exp)))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-consequent exp) (car (cdr exp)))
(define (if-fail-alternate exp) (car (cdr (cdr exp))))

(define (analyze-if-fail exp)
  (let ((cproc (analyze (if-fail-consequent exp)))
        (aproc (analyze (if-fail-alternate exp))))
    (lambda (env succeed fail)
      (cproc env succeed (lambda () (aproc env succeed fail))))))

(-> (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items)))))

(-> (if-fail (let ((x (an-element-of '(1 3 5))))
	       (require (even? x))
	       x)
	     'all-odd)) ; all-odd

(-> (if-fail (let ((x (an-element-of '(1 3 5 8))))
	       (require (even? x))
	       x)
	     'all-odd)) ; 8

; Exercise 4.53 ------------------------------------------------------------------

(-> (define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
	    (b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b))))

(-> (define (square n)
      (* n n)))
(-> (define (smallest-divisor n)
      (find-divisor n 2)))
(-> (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
	    (( divides? test-divisor n) test-divisor)
	    (else (find-divisor n (+ test-divisor 1))))))
(-> (define (divides? a b)
      (= (remainder b a) 0)))
(-> (define (prime? n)
      (= n (smallest-divisor n))))

(-> (let ((pairs '()))
      (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
		 (permanent-set! pairs (cons p pairs))
		 (amb))
	       pairs)))

; ((8 35) (3 110) (3 20))

; Exercise 4.54 ------------------------------------------------------------------

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
(define prev-analyze4 analyze)
(define (analyze exp)
  (if (require? exp)
      (analyze-require exp)
      (prev-analyze4 exp)))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail)
                   (succeed 'ok fail2)))
             fail))))

(-> (let ((x (an-element-of '(1 2 3 4))))
      (require (> x 2))
      x)) ; 3
