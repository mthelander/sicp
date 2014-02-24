; Exercise 3.12 ------------------------------------------------------------------------------
;
; 1. (b)
;
; The non-mutating version of append is used on x, therefore it remains unchanged:
;
; x-->()()-->()()-->
;     |      |
;     a      b
;
; 2. (b c d)
;
; x-->()()-->()()-->()()-->()()-->
;     |      |      |      |
;     a      b      c      d      


; Exercise 3.13 ------------------------------------------------------------------------------
;
;     +-------------------+
;     |                   |
; z-->()()-->()()-->()()--+
;     |      |      |     
;     a      b      c
;
; If we try to compute (last-pair z) then it will recurse indefinitely.


; Exercise 3.14 ------------------------------------------------------------------------------
;
; It reverses a list.
;
; v-->()()-->()()-->()()-->()()-->
;     |      |      |      |
;     a      b      c      d      
;
; (define w (mystery v))
;
; v-->()()-->
;     |
;     a
;
; w-->()()-->()()-->()()-->()()-->
;     |      |      |      |
;     d      c      b      a      


; Exercise 3.15 ------------------------------------------------------------------------------
;
; z1-->()()
;      |  |
;      | /
;      ()()-->()()-->
;      |      |
;     wow     b
;
;
; z2-->()()--------------->()()-->()()-->
;      |                   |      |
;      ()()-->()()-->      a      b
;      |      |
;     wow     b


; Exercise 3.16 ------------------------------------------------------------------------------
;
; x--->()()-->()()-->()()-->
;      |      |      |
;      a      b      c
;
; x--->()()
;      |  |
;      | /
;      ()()-->()()-->
;      |      |
;      a      b
;

; Exercise 3.17 ------------------------------------------------------------------------------

(define (count-pairs-bad x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (count-pairs x)
  (define (seen? in e) (memq e in))
  (define (iter x prev-seen)
    (cond ((pair? x) 0)
	  ((seen? prev-seen (car x)) 0)
	  (else (+ (iter (car x) (cons (car x) prev-seen))
		   (iter (cdr x) prev-seen))
		   1)))
  (iter x '()))

(define x (list 'a 'b 'c))
(define y (list 'q 'y x 'z x))

;(count-pair y)
;(count-pair-bad y)

; Exercise 3.18 -- SKIPPED
; Exercise 3.19 -- SKIPPED
; Exercise 3.20 -- SKIPPED

; Exercise 3.21 ------------------------------------------------------------------------------

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; My code

; The results look that way because the queue is represented as a pair of pointers.

;; ((a) a) ; Here the 'a' is added into both the front and the back of the queue
;; ((a b) b) ; The 'b' is added to the back of the queue
;; ((b) b) ; The 'a' is deleted from the front of the queue
;; (() b) ; The 'b' is deleted from the front of the queue

(define (print-queue queue)
  (pp (front-ptr queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(insert-queue! q1 'c)
(print-queue q1) ; (a b c)

; Exercise 3.22 ------------------------------------------------------------------------------

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" queue)
	  (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty queue"))
	    (else (set! front-ptr (cdr front-ptr)))))

    (define (print-queue)
      (pp front-ptr))

    (define (dispatch m)
      (case m
	((empty-queue?) empty-queue?)
	((front-queue) front-queue)
	((insert-queue!) insert-queue!)
	((delete-queue!) delete-queue!)
	((print-queue) print-queue)
	(else (error "Invalid operation -- " m))))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'print-queue))
((q1 'insert-queue!) 'b)
((q1 'print-queue))
((q1 'insert-queue!) 'c)
((q1 'print-queue)) ; (a b c)
((q1 'front-queue)) ; a

((q1 'delete-queue!))
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue)) ; ()

; Exercise 3.23 ------------------------------------------------------------------------------

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-deque?) (null? front-ptr))

    (define (front-deque)
      (if (empty-deque?)
	  (error "FRONT called with an empty deque" deque)
	  (car front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
	  (error "REAR called with an empty deque" deque)
	  rear-ptr))

    (define (front-insert-deque! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-deque?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! new-pair front-ptr)
	       (set! front-ptr new-pair)))))
    
    (define (rear-insert-deque! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-deque?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))

    (define (front-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty deque"))
	    (else (set! rear-ptr (cdr rear-ptr)))))

    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty deque"))
	    (else (set! front-ptr (cdr front-ptr))))) ; <---- wrong wrong wrong

    (define (print-deque)
      (pp front-ptr))

    (define (dispatch m)
      (case m
	((empty-deque?) empty-deque?)
	((front-deque) front-deque)
	((rear-deque) rear-deque)
	((front-insert-deque!) front-insert-deque!)
	((rear-insert-deque!) rear-insert-deque!)
	((delete-front-deque!) delete-front-deque!)
	((delete-rear-deque!) delete-rear-deque!)
	((print-deque) print-deque)
	(else (error "Invalid operation -- " m))))
    dispatch))

(define q1 (make-deque))
((q1 'rear-insert-deque!) 'a)
((q1 'print-deque))
((q1 'rear-insert-deque!) 'b)
((q1 'print-deque))
((q1 'rear-insert-deque!) 'c)
((q1 'print-deque)) ; (a b c)
((q1 'front-deque)) ; a

;; (define (front-ptr deque) (car deque))
;; (define (rear-ptr deque) (cdr deque))
;; (define (set-front-ptr! deque item) (set-car! deque item))
;; (define (set-rear-ptr! deque item) (set-cdr! deque item))

;; (define (prev item) (cadr item))
;; (define (next item) (caadr item))
;; (define (curr item) (car item))

;; (define (empty-deque? deque) (null? (front-ptr deque)))

;; (define (make-deque) (cons '() '()))

;; (define (front-deque deque)
;;   (if (empty-deque? deque)
;;       (error "FRONT called with an empty deque" deque)
;;       (curr (front-ptr deque))))

;; (define (rear-deque deque)
;;   (if (empty-deque? deque)
;;       (error "FRONT called with an empty deque" deque)
;;       (curr (rear-ptr deque))))

;; (define (front-insert-deque! deque item)
;;   (let ((new-pair (list item '() '())))
;;     (cond ((empty-deque? deque)
;;            (set-front-ptr! deque new-pair)
;;            (set-rear-ptr! deque new-pair)
;;            deque)
;;           (else
;; 	   ;(pp new-pair)
;; 	   (set-cdr! (cdr new-pair) (front-ptr deque))
;; 	   (set-front-ptr! deque new-pair)
;;            deque))))

;; (define (rear-insert-deque! deque item)
;;   (let ((new-pair (cons item '())))
;;     (cond ((empty-deque? deque)
;;            (set-front-ptr! deque new-pair)
;;            (set-rear-ptr! deque new-pair)
;;            deque)
;;           (else
;; 	   (set-cdr! rear-ptr new-pair)
;; 	   (set-rear-ptr! new-pair)
;;            deque))))

;; ;; (define (front-delete-deque! deque)
;; ;;   (cond ((empty-deque? deque)
;; ;;          (error "DELETE! called with an empty deque" deque))
;; ;;         (else
;; ;;          (set-front-ptr! deque (cdr (front-ptr deque)))
;; ;;          deque)))

;; (define (print-deque deque)
;;   (define (iter elements)
;;     (cond ((not (null? elements))
;; 	   (display (curr (car elements)))
;; 	   (iter (cdr elements)))))
;;   (iter (front-deque deque)))

;; (display "-------------------------------------------------")

;; (define q1 (make-deque))
;; (front-insert-deque! q1 'a) ; ((a) a)
;; (front-insert-deque! q1 'b) ; ((b a) a)


;; ((q1 'print-deque))

;; ((q1 'front-insert-deque!) 'c) ; (c b a)
;; ((q1 'print-deque))

;; ((q1 'front-deque)) ; c



;; ((q1 'delete-queue!))
;; ((q1 'print-queue))
;; ((q1 'delete-queue!))
;; ((q1 'print-queue))
;; ((q1 'delete-queue!))
;; ((q1 'print-queue)) ; ()


; Exercise 3.24 ------------------------------------------------------------------------------

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table1 (make-table equal?))
((table1 'insert-proc!) 'foo 'bar 1223)
((table1 'lookup-proc) 'foo 'bar)

; Exercise 3.25 ------------------------------------------------------------------------------

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define (iter record keys)
        (cond ((null? keys)
               (if record (cdr record) false))
              (else (iter (assoc (car keys) (cdr record)) (cdr keys)))))
      (iter local-table keys))
    (define (add-key-value record key value)
      (set-cdr! record
		(cons (cons key value)
		      (cdr record))))
    (define (insert! keys value)
      (define (iter subtable keys)
	(let ((record (assoc (car keys) (cdr subtable))))
	  (cond ((eq? (length keys) 1)
		 (if record
		     (set-cdr! record value)
		     (add-key-value subtable (car keys) value)))
		(record
		 (iter record (cdr keys)))
		(else
		 (let ((new-subtable (make-table same-key?)))
		   ((new-subtable 'insert-proc!) (cdr keys) value)
		   (add-key-value subtable (car keys) ((new-subtable 'get-records))))))))
      (iter local-table keys)
      'ok)
    (define (get-records)
      (cdr local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'get-records) get-records)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table1 (make-table equal?))
((table1 'insert-proc!) '(foo bar baz) 1223) ; ok
((table1 'insert-proc!) '(foo bar bak) 514) ; ok
((table1 'insert-proc!) '(foo qux) 723) ; ok
((table1 'lookup-proc) '(foo bar baz)) ; 1223
((table1 'lookup-proc) '(foo bar bak)) ; 514
((table1 'lookup-proc) '(foo qux)) ; 723

; Exercise 3.26 ------------------------------------------------------------------------------

; If we structured the key-value pairs as a bst then we could perform lookups faster in O(logn) time instead of O(n), at the expense of increasing the time for insertions to O(logn) instead of O(1). The biggest change to our code would be to alter assoc to traverse the bst instead of sequentially iterating over a list.

; Exercise 3.27 ------------------------------------------------------------------------------

; memo-fib computes the nth fibonacci number in O(n) time because it eliminates the repeated computations.
;             +----------+
; +-------------+        |
; | memo-fib: --+----->()()
; +-------------+       |
;                       +--> Parameters: x
;                            Body: (let ((previously....)))
; SKIPPED

; Section 3.34 -------------------------------------------------------------------------------

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define (logical-and a b)
  (if (and (= a 1) (= b 1)) 1 0))

; Queue code from 3.3.2

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

; The agenda

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; Exercise 3.28 ------------------------------------------------------------------------------

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or a b)
  (if (eq? a 1) 1 b))

; Exercise 3.29 ------------------------------------------------------------------------------

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (and-gate a1 a2 output)
    (if (eq? 0 output)
	(let ((na1 (inverter a1)) (inverter (na2 0)))
	  (and-gate a1 na2 output)
	  (if (eq? 0 output)
	      (and-gate na1 a2 output)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; The delay time is (+ (* 3 and-delay) (* 2 inverter-delay))

; Exercise 3.30 ------------------------------------------------------------------------------

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or a b)
  (if (eq? a 1) 1 b))

(define (ripple-carry-adder a b s c)
  (if (not (there-exists? (list a b s) null?)) 
      (let ((an (car a)) (bn (car bn)) (sn (car sn)))
	(full-adder an bn sn c)
	(ripple-carry-adder (cdr a) (cdr b) (cdr s) c))))

; The delay is (* n (+ or-delay inverter-delay (* 2 and-delay)))

; Exercise 3.31 ------------------------------------------------------------------------------

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
; sum 0  New-value = 0
(probe 'carry carry)
; carry 0  New-value = 0

; 
(half-adder input-1 input-2 sum carry)
; ok
(set-signal! input-1 1)
; done
(propagate)
; sum 8  New-value = 1
; done

; The procedure must be called immediately because otherwise after-delay is never invoked,
; so the action is never added to the agenda (and is never run by propagate)

; Exercise 3.32 ------------------------------------------------------------------------------

; The first-in-first-out order must be used because that's the order in which the actions where evaluated. If we use last-in-first-out order then the output could be left in a state that is not accurate.

; Start:
; a1 = 0
; a2 = 1

; Change:
; a1 = 1
; a2 = 0

; 2 actions on the agenda for that time segment:
;   1. new-value is 1&1=1; set output to 1 after delay
;   2. new-value is 1&0=0; set output to 0 after delay

; Because the lambdas that set the output are also evaluated in first-in-first-out order, the output is correct; 0.
; If it were evaluated in last-in-first-out order, then output would be 1 -- not correct.

; Section 3.3.5

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; Exercise 3.33 ------------------------------------------------------------------------------

(define (averager a b c)
  (let ((sum (make-connector))
	(avg (make-connector)))
    (adder a b sum)
    (multiplier avg sum c)
    (constant 1/2 avg))
  'ok)

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "var a" a)
(probe "var b" b)
(probe "average" c)

(averager a b c)

(set-value! a 32 'user)
; Probe: var a = 32
(set-value! b 28 'user)
; Probe: average = 30
; Probe: var b = 28

; Exercise 3.34 ------------------------------------------------------------------------------

; Louis' suggestion lacks the nondirectionality that characterizes our primitive functions. In other words, you cannot infer the value of a given only b.

; Exercise 3.35 ------------------------------------------------------------------------------

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt b) me))
        (if (has-value? a)
	    (set-value! b (expt (get-value a) 2) me)))
    me)
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))

(probe "var a" a)
(probe "var b" b)

(squarer a b)

(set-value! a 8 'user)
; Probe: var b = 64
; Probe: var a = 8

; Exercise 3.36 ------------------------------------------------------------------------------

; DO THIS

; Exercise 3.37 ------------------------------------------------------------------------------

(define (c- x y)
  (let ((negator (make-connector))
	(negative-y (make-connector))
	(diff (make-connector)))
    (constant -1 negator)
    (multiplier y negator negative-y)
    (adder x negative-y diff)
    diff))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((reciprocal (make-connector))
	(quotient (make-connector)))
    (constant (/ 1 (get-value y)) reciprocal)
    (multiplier x reciprocal quotient)
    quotient))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celcius" C)
(probe "Fahrenheit" F)

(set-value! F 212 'user)
; Probe: Fahrenheit = 212
; Probe: Celcius = 100

(forget-value! F 'user)
(set-value! C 10 'user)
; Probe: Celcius = 10
; Probe: Fahrenheit = 50
