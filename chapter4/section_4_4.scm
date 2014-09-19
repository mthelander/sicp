; Section 4.4.1 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch4-query.scm")

(define (run-query query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))))))

(initialize-data-base microshaft-data-base) ; done

; Exercise 4.55 ------------------------------------------------------------------

; a. all people supervised by Ben Bitdiddle;

(run-query '(supervisor ?x (Bitdiddle Ben)))
;; (supervisor (tweakit lem e) (bitdiddle ben))
;; (supervisor (fect cy d) (bitdiddle ben))
;; (supervisor (hacker alyssa p) (bitdiddle ben))

; b. the names and jobs of all people in the accounting division;

(run-query '(job ?name (accounting . ?job)))
;; (job (cratchet robert) (accounting scrivener))
;; (job (scrooge eben) (accounting chief accountant))

; c. the names and addresses of all people who live in Slumerville.

(run-query '(address ?name (Slumerville . ?address)))
;; (address (aull dewitt) (slumerville (onion square) 5))
;; (address (reasoner louis) (slumerville (pine tree road) 80))
;; (address (bitdiddle ben) (slumerville (ridge road) 10))

; Exercise 4.56 ------------------------------------------------------------------

; a. the names of all people who are supervised by Ben Bitdiddle, together with
; their addresses;

(run-query '(and (supervisor ?person (Bitdiddle Ben))
		 (address ?person ?where)))
;; (and (supervisor (tweakit lem e) (bitdiddle ben))
;;      (address (tweakit lem e) (boston (bay state road) 22)))
;; (and (supervisor (fect cy d) (bitdiddle ben))
;;      (address (fect cy d) (cambridge (ames street) 3)))
;; (and (supervisor (hacker alyssa p) (bitdiddle ben))
;;      (address (hacker alyssa p) (cambridge (mass ave) 78)))

; b. all people whose salary is less than Ben Bitdiddle's, together with their
; salary and Ben Bitdiddle's salary;

(run-query '(and (salary (Bitdiddle Ben) ?bsalary)
		 (salary ?person ?salary)
		 (lisp-value < ?salary ?bsalary)))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (aull dewitt) 25000)
;;      (lisp-value < 25000 60000))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (cratchet robert) 18000)
;;      (lisp-value < 18000 60000))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (reasoner louis) 30000)
;;      (lisp-value < 30000 60000))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (tweakit lem e) 25000)
;;      (lisp-value < 25000 60000))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (fect cy d) 35000)
;;      (lisp-value < 35000 60000))
;; (and (salary (bitdiddle ben) 60000)
;;      (salary (hacker alyssa p) 40000)
;;      (lisp-value < 40000 60000))

; c. all people who are supervised by someone who is not in the computer
; division, together with the supervisor's name and job.

(run-query '(and (supervisor ?person ?super)
		 (not (job ?super (computer . ?job)))
		 (job ?super ?superjob)))
;; (and (supervisor (aull dewitt) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?job)))
;;      (job (warbucks oliver) (administration big wheel)))
;; (and (supervisor (cratchet robert) (scrooge eben))
;;      (not (job (scrooge eben) (computer . ?job)))
;;      (job (scrooge eben) (accounting chief accountant)))
;; (and (supervisor (scrooge eben) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?job)))
;;      (job (warbucks oliver) (administration big wheel)))
;; (and (supervisor (bitdiddle ben) (warbucks oliver))
;;      (not (job (warbucks oliver) (computer . ?job)))
;;      (job (warbucks oliver) (administration big wheel)))

; Exercise 4.57 ------------------------------------------------------------------

(run-query '(assert! (rule (can-replace ?person-1 ?person-2)
			   (and (job ?person-1 ?job-1)
				(job ?person-2 ?job-2)
				(or (same ?job-1 ?job-2)
				    (can-do-job ?job-1 ?job-2))
				(not (same ?person-1 ?person-2))))))

; a. all people who can replace Cy D. Fect;

(run-query '(can-replace ?person (Fect Cy D)))
;; (can-replace (bitdiddle ben) (fect cy d))
;; (can-replace (hacker alyssa p) (fect cy d))

; b. all people who can replace someone who is being paid more than they are, together with the two salaries.

(run-query '(and (salary ?person-1 ?salary-1)
		 (salary ?person-2 ?salary-2)
		 (lisp-value < ?salary-1 ?salary-2)
		 (can-replace ?person-1 ?person-2)))
;; (and (salary (aull dewitt) 25000)
;;      (salary (warbucks oliver) 150000)
;;      (lisp-value < 25000 150000)
;;      (can-replace (aull dewitt) (warbucks oliver)))
;; (and (salary (fect cy d) 35000)
;;      (salary (hacker alyssa p) 40000)
;;      (lisp-value < 35000 40000)
;;      (can-replace (fect cy d) (hacker alyssa p)))

; Exercise 4.58 ------------------------------------------------------------------

(run-query '(assert! (rule (big-shot ?person ?division)
			   (and (job ?person (?division . ?job))
				(not (and (supervisor ?person ?supervisor)
					  (job ?supervisor (?division . ?superjob))))))))

(run-query '(big-shot ?x ?y))
;; (big-shot (scrooge eben) accounting)
;; (big-shot (warbucks oliver) administration)
;; (big-shot (bitdiddle ben) computer)

; Exercise 4.59 ------------------------------------------------------------------

(run-query '(assert! (meeting accounting (Monday 9am))))
(run-query '(assert! (meeting administration (Monday 10am))))
(run-query '(assert! (meeting computer (Wednesday 3pm))))
(run-query '(assert! (meeting administration (Friday 1pm))))

(run-query '(assert! (meeting whole-company (Wednesday 4pm))))

; a.

(run-query '(meeting ?division (Friday ?time)))

; b.

(run-query '(assert! (rule (meeting-time ?person ?day-and-time)
			   (and (job ?person (?division . ?job))
				(or (meeting whole-company ?day-and-time)
				    (meeting ?division ?day-and-time))))))

(run-query '(meeting-time (Hacker Alyssa P) ?day-and-time))
;; (meeting-time (hacker alyssa p) (wednesday 4pm))
;; (meeting-time (hacker alyssa p) (wednesday 3pm))

; c.

(run-query '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))
;; (meeting-time (hacker alyssa p) (wednesday 4pm))
;; (meeting-time (hacker alyssa p) (wednesday 3pm))

; Exercise 4.60 ------------------------------------------------------------------

; This happens because the candidate pool for each person variable is the entire
; database of people, and the call to (same) only prevents them from being the
; same person and doesn't care about the order. 

; We could modify it to work as Alyssa expects by adding a lisp-value line to
; the rule that enforces an arbitrary order on the pair, which will only allow
; half of the total pairs to filter through.

(define (list<? a b)
  (apply symbol<? (map car (list a b))))

(run-query '(assert! (rule (lives-near-pairs ?person-1 ?person-2)
			   (and (address ?person-1 (?town . ?rest-1))
				(address ?person-2 (?town . ?rest-2))
				(not (same ?person-1 ?person-2))
				(lisp-value list<? ?person-1 ?person-2)))))

(run-query '(lives-near-pairs ?person-1 ?person-2))
;; (lives-near-pairs (aull dewitt) (reasoner louis))
;; (lives-near-pairs (aull dewitt) (bitdiddle ben))
;; (lives-near-pairs (fect cy d) (hacker alyssa p))
;; (lives-near-pairs (bitdiddle ben) (reasoner louis))

; Exercise 4.61 ------------------------------------------------------------------

(run-query '(assert! (rule (?x next-to ?y in (?x ?y . ?u)))))
(run-query '(assert! (rule (?x next-to ?y in (?v . ?z))
			   (?x next-to ?y in ?z))))

(run-query '(?x next-to ?y in (1 (2 3) 4)))
;; ((2 3) next-to 4 in (1 (2 3) 4))
;; (1 next-to (2 3) in (1 (2 3) 4))

(run-query '(?x next-to 1 in (2 1 3 1)))
;; (3 next-to 1 in (2 1 3 1))
;; (2 next-to 1 in (2 1 3 1))

; Exercise 4.62 ------------------------------------------------------------------

(run-query '(assert! (rule (last-pair (?x) (?x)))))
(run-query '(assert! (rule (last-pair (?y . ?u) (?z))
			   (last-pair ?u (?z)))))

(run-query '(last-pair (3) ?x)) ; (last-pair (3) (3))
(run-query '(last-pair (1 2 3) ?x)) ; (last-pair (1 2 3) (3))
(run-query '(last-pair (2 ?x) (3))) ; (last-pair (2 3) (3))

;(run-query '(last-pair ?x (3)))
; The last one never returns... This suggests to me that there are an infinite
; number of solutions.

; Exercise 4.63 ------------------------------------------------------------------

(run-query '(assert! (son Adam Cain)))
(run-query '(assert! (son Cain Enoch)))
(run-query '(assert! (son Enoch Irad)))
(run-query '(assert! (son Irad Mehujael)))
(run-query '(assert! (son Mehujael Methushael)))
(run-query '(assert! (son Methushael Lamech)))
(run-query '(assert! (wife Lamech Ada)))
(run-query '(assert! (son Ada Jabal)))
(run-query '(assert! (son Ada Jubal)))

;; (run-query '(assert! (rule (grandson ?grandparent ?grandson)
;; 			   (and (or (son ?grandparent ?parent)
;; 				    (stepson ?grandparent ?parent))
;; 				(or (son ?parent ?grandson)
;; 				    (stepson ?parent ?grandson))))))
(run-query '(assert! (rule (grandson ?gp ?gs)
			   (and (son-of ?gp ?p)
				(son-of ?p ?gs)))))

(run-query '(assert! (rule (stepson ?w ?s)
			   (and (wife ?w ?f)
				(son ?f ?s)))))

(run-query '(assert! (rule (son-of ?p ?s)
			   (or (son ?p ?s)
			       (stepson ?p ?s)))))

(run-query '(grandson Cain ?gs))
;; (grandson cain irad)
(run-query '(stepson Lamech ?s))
;; (stepson lamech jubal)
;; (stepson lamech jabal)
(run-query '(grandson Methushael ?gs))
;; (grandson methushael jubal)
;; (grandson methushael jabal)

; Exercise 4.64 ------------------------------------------------------------------

; It starts out with an OR expression, which simply executes each statement in
; parallel. The first statement finds the expected result, which is why it
; returns a result first.

; The second expression however, is an AND expression; normally, the database
; would be scanned for all assertions that satisfy the pattern (supervisor...),
; then it would use that output stream in the recursive application of the
; outranked-by rule. But, since the recursive application is first in the AND
; expression, it results in an infinite loop.

; Exercise 4.65 ------------------------------------------------------------------

;(run-query '(supervisor ?subordinate ?supervisor))
;; (supervisor (aull dewitt)     (warbucks oliver))
;; (supervisor (cratchet robert) (scrooge eben))
;; (supervisor (scrooge eben)    (warbucks oliver))
;; (supervisor (bitdiddle ben)   (warbucks oliver))
;; (supervisor (reasoner louis)  (hacker alyssa p))
;; (supervisor (tweakit lem e)   (bitdiddle ben))
;; (supervisor (fect cy d)       (bitdiddle ben))
;; (supervisor (hacker alyssa p) (bitdiddle ben))

;; ((?middle-manager (aull dewitt))     (?person (warbucks oliver)))
;; ((?middle-manager (cratchet robert)) (?person (scrooge eben)))
;; ((?middle-manager (scrooge eben))    (?person (warbucks oliver)))
;; ((?middle-manager (bitdiddle ben))   (?person (warbucks oliver)))
;; ((?middle-manager (reasoner louis))  (?person (hacker alyssa p)))
;; ((?middle-manager (tweakit lem e))   (?person (bitdiddle ben)))
;; ((?middle-manager (fect cy d))       (?person (bitdiddle ben)))
;; ((?middle-manager (hacker alyssa p)) (?person (bitdiddle ben)))

;; ((?x (crachet robert)) (?middle-manager (scrooge eben))    (?person (warbucks oliver)))
;; ((?x tweakit lem e))   (?middle-manager (bitdiddle ben))   (?person (warbucks oliver)))
;; ((?x fect cy d))       (?middle-manager (bitdiddle ben))   (?person (warbucks oliver)))
;; ((?x hacker alyssa p)) (?middle-manager (bitdiddle ben))   (?person (warbucks oliver)))
;; ((?x (reasoner loius)) (?middle-manager (hacker alyssa p)) (?person (bitdiddle ben)))

;; (rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;; 	   (supervisor ?x ?middle-manager)))

; It first loops through all the assertions, which produces a stream of frames
; with bindings for ?middle-manager and ?person.

; That stream of frames is then used as the input stream of frames for the second
; and expression such that ?middle-manager is consistent with the previous
; expression.

; This seems like an inner join in SQL.

; Oliver Warbucks is listed four times because there are four supervisor paths that begin with
; him:
; 
; (warbucks oliver) -> (dewitt aull)
; (warbucks oliver) -> (scrooge eben)  -> (cratchet robert)
; (warbucks oliver) -> (bitdiddle ben) -> (tweakit lem e)
; (warbucks oliver) -> (bitdiddle ben) -> (fect cy d)
; (warbucks oliver) -> (bitdiddle ben) -> (hacker alyssa p) -> (reasoner louis)

; Exercise 4.66 ------------------------------------------------------------------

; Ben has realized that there will be duplicates records in the stream of frames,
; just like with the previous exercise.

; One way he could salvage the situation would be to implement a group-by procedure,
; which he could then use to merge the duplicate fields.

; Exercise 4.67 ------------------------------------------------------------------

; A simple way to prevent loops would be to maintain a global list of current rule
; applications, then modify the apply-a-rule procedure so that it inserts a new
; list of the unifying result and the rule name using (put), but exits early if
; get already returns that list.

; Exercise 4.68 ------------------------------------------------------------------

(run-query '(assert! (rule (append-to-form () ?y ?y))))
(run-query '(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
			   (append-to-form ?v ?y ?z))))

(run-query '(assert! (rule (reverse (?x) (?x)))))
(run-query '(assert! (rule (reverse (?x . ?y) ?z)
			   (and (reverse ?y ?u)
				(append-to-form ?u (?x) ?z)))))

(run-query '(reverse (1 2 3) ?x)) ; (reverse (1 2 3) (3 2 1))
;(run-query '(reverse ?b (1 2 3))) ; This never returns

; Exercise 4.69 ------------------------------------------------------------------

;; (run-query '(assert! (rule (ends-with-grandson (grandson)))))
;; (run-query '(assert! (rule (ends-with-grandson (?x . ?y))
;; 			   (ends-with-grandson ?y))))
;; (run-query '(assert! (rule (ends-with-grandson ?list)
;; 			   (and (last-pair ?list ?end)
;; 				(same ?end (grandson))))))
(run-query '(assert! (rule (ends-with-grandson ?list)
			   (append-to-form ?front (grandson) ?list))))
(run-query '(assert! (rule ((grandson) ?gp ?gs)
			   (grandson ?gp ?gs))))
(run-query '(assert! (rule ((great . ?rel) ?ggp ?ggs)
			   (and (son-of ?ggp ?gp)
				(?rel ?gp ?ggs)
				(ends-with-grandson ?rel)))))

; (Adam -> Cain -> Enoch -> Irad -> Mehujael -> Methusael -> Lamech )

(run-query '(ends-with-grandson (great great grandson)))
;; (ends-with-grandson (great great grandson))

(run-query '((great grandson) ?g ?ggs))
;; ((great grandson) mehujael jubal)
;; ((great grandson) irad lamech)
;; ((great grandson) mehujael jabal)
;; ((great grandson) enoch methushael)
;; ((great grandson) cain mehujael)
;; ((great grandson) adam irad)

(run-query '((great grandson) Adam ?who))
;; ((great grandson) adam irad)
(run-query '((great great grandson) Adam ?who))
;; ((great great grandson) adam mehujael)
(run-query '((great great great great great grandson) Adam ?who))
;; ((great great great great great grandson) adam jubal)
;; ((great great great great great grandson) adam jabal)

(run-query '(?relationship Adam Irad))
;; ((great grandson) adam irad)
(run-query '(?relationship Adam Enoch)) 
;; (grandson adam enoch) ((grandson) adam enoch)
(run-query '(?relationship Adam Lamech))
;; ((great great great great grandson) adam lamech)

; Exercise 4.70 ------------------------------------------------------------------

; That would create an infinite stream of the assertion because the set!
; expression is evaluated from left to right. The let expression creates
; a separate binding to the value of the globally defined stream, therefore
; it retains its value throughout the set! expression.

; Exercise 4.71 ------------------------------------------------------------------

;; The only disadvantage that I see is that with the delay operations potential
;; infinite loops are deferred until after the assertions are processed, which
;; means that we could get output before it dies, rather then just dying without
;; any output at all. This doesn't seem like much of a win however.

;; (run-query '(assert! (yak-shaver (Bitdiddle Ben))))
;; (run-query '(assert! (rule (yak-shaver ?wat)
;; 			   (yak-shaver ?wat))))
;; (run-query '(yak-shaver ?wat))

;; With delays:

;;; Query results:
;; (yak-shaver (bitdiddle ben))
;; (yak-shaver (bitdiddle ben))
;; (yak-shaver (bitdiddle ben))
;; ...
;Aborting!: maximum recursion depth exceeded

;; Without delays

;;; Query results:
;Aborting!: maximum recursion depth exceeded

; Exercise 4.72 ------------------------------------------------------------------

;; They interleave the streams to handle infinite streams. Interleaving guarantees
;; that every element in each stream will eventually be displayed, given infinite
;; time. 

;; TODO: Example?

; Exercise 4.73 ------------------------------------------------------------------

; Just like with the previous two exercises, delay is used to handle deferring
; infinite loops. If we wrote it without the explicit delay operations then it
; would never progress beyond the first stream of streams if we reach an infinite
; loop, it would simply loop forever without returning anything.

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave
;;        (stream-car stream)
;;        (flatten-stream (stream-cdr stream)))))

; Exercise 4.74 ------------------------------------------------------------------

; a.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
	      (stream-filter (lambda (x) (not (stream-null? x))) stream)))

; b.

; It's not quite the same; since we're no longer delaying the interleaving we're
; forced to evaluate the entire stream, which may not be desirable if it is
; infinite.

; Exercise 4.75 ------------------------------------------------------------------

(define (uniquely-asserted pattern frames)
  (stream-flatmap
   (lambda (frame)
     (let ((extensions (qeval (car pattern)
			      (singleton-stream frame))))
       (if (= (stream-length extensions) 1)
	   (singleton-stream (stream-car extensions))
	   the-empty-stream)))
   frames))

(put 'unique 'qeval uniquely-asserted)

(run-query '(unique (job ?x (computer wizard))))
;; ;;; Query results:
;; (unique (job (bitdiddle ben) (computer wizard)))
;; ;Value: done

(run-query '(unique (job ?x (computer programmer))))
;;; Query results:
;Value: done

(run-query '(and (job ?x ?j) (unique (job ?anyone ?j))))
;; ;;; Query results:
;; (and (job (aull dewitt) (administration secretary))
;;      (unique (job (aull dewitt) (administration secretary))))
;; (and (job (cratchet robert) (accounting scrivener))
;;      (unique (job (cratchet robert) (accounting scrivener))))
;; (and (job (scrooge eben) (accounting chief accountant))
;;      (unique (job (scrooge eben) (accounting chief accountant))))
;; (and (job (warbucks oliver) (administration big wheel))
;;      (unique (job (warbucks oliver) (administration big wheel))))
;; (and (job (reasoner louis) (computer programmer trainee))
;;      (unique (job (reasoner louis) (computer programmer trainee))))
;; (and (job (tweakit lem e) (computer technician))
;;      (unique (job (tweakit lem e) (computer technician))))
;; (and (job (bitdiddle ben) (computer wizard))
;;      (unique (job (bitdiddle ben) (computer wizard))))
;; ;Value: done

(run-query '(and (job ?supervisor ?job)
		 (unique (supervisor ?subordinate ?supervisor))))
;; ;;; Query results:
;; (and (job (scrooge eben) (accounting chief accountant))
;;      (unique (supervisor (cratchet robert) (scrooge eben))))
;; (and (job (hacker alyssa p) (computer programmer))
;;      (unique (supervisor (reasoner louis) (hacker alyssa p))))
;; ;Value: done

; Exercise 4.76 ------------------------------------------------------------------

(define (conjoin conjuncts frame-stream)
  (define (merge-frames a b)
    (if (null? a) b
	(merge-frames (cdr a)
		      (extend-if-consistent (binding-variable (car a))
					    (binding-value (car a))
					    b))))

  (define (merge-frame-streams frames1 frames2)
    (stream-filter (lambda (x) (not (eq? x 'failed)))
		   (stream-flatmap (lambda (a)
				     (stream-map (lambda (b) (merge-frames a b))
						 frames2))
				   frames1)))
  (let ((evaled-conjuncts (map (lambda (c) (qeval c frame-stream)) conjuncts)))
    (reduce merge-frame-streams
	    (car evaled-conjuncts)
	    (cdr evaled-conjuncts))))

(put 'and 'qeval conjoin)

(run-query '(and (supervisor ?x (Bitdiddle Ben))
		 (job ?x (computer programmer))))
;; (and (supervisor (fect cy d) (bitdiddle ben))
;;      (job (fect cy d) (computer programmer)))
;; (and (supervisor (hacker alyssa p) (bitdiddle ben))
;;      (job (hacker alyssa p) (computer programmer)))

; Exercise 4.77 ------------------------------------------------------------------

; One way we could accomplish this is to make (negate) and (lisp-value) return the
; stream of frames from the expression, but tag them in some way so that we can
; then modify (check-an-assertion) to invert the logic for including frames when
; that tag is present. 

; Exercise 4.78 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch4-ambeval.scm")

(define (analyze exp)
  (cond ((conjunction? exp) (analyze-conjunction exp))
	((disjunction? exp) (analyze-disjunction exp))
        ((negation? exp) (analyze-negation exp))
        ((lisp-value? exp) (analyze-lisp-value exp))
	((simple-query? exp) (analyze-query exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (conjunction? exp) (tagged-list? exp 'and))
(define (disjunction? exp) (tagged-list? exp 'or))
(define (negation? exp) (tagged-list? exp 'not))
(define (lisp-value? exp) (tagged-list? exp 'lisp-value))
(define (simple-query? exp) (application? exp))

(define (analyze-assertion pattern assertion)
  (let ((match-result (check-an-assertion pattern
					  assertion
					  (singleton-stream '()))))
    (lambda (env succeed fail)
      (if (stream-null? match-result)
	  (fail)
	  (succeed match-result fail)))))

(define (query-choices pattern)
  (let ((index (car pattern)))
    (filter (lambda (a) (eq? (car a) index))
	    (stream->list (get-all-assertions)))))

(define (analyze-query pattern)
  (let ((cprocs (map (lambda (a) (analyze-assertion pattern a))
		     (query-choices pattern))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define the-global-environment (setup-environment))

(define (run exp)
  (ambeval exp the-global-environment
	   (lambda (val next) val)
	   (lambda () (display "fail"))))

(run '(job ?who (computer wizard)))
; (job (bitdiddle ben) (computer wizard))
