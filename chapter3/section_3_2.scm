; Exercise 3.9 --------------------------------------------------
;
;
;                +--------------------------------------------------------------------------+ <---------+
;                |                                                                          |           |
; Global env --> | factorial:---------------------------------------------------------------+------> ()()
;                |                                                                          |        |
;                +--------------------------------------------------------------------------+	     |
; (factorial 6)    ^   	    	^	     ^	          ^            ^            ^                +---------> Parameters: n
;                  |   	       	|      	     |		  |	       |            |                            Body: (if (= n 1) 1 (* n (factorial (- n 1))))
;		   |		|	     |		  |	       |	    |
;      	       	   |		|	     |		  |	       |	    |
;                  |   	       	|      	     | 	       	  |    	       |       	    |
;               +------+     +------+  	  +------+     +------+	    +------+   	 +------+
;              	| n: 6 |     | n: 5 |  	  | n: 4 |     | n: 3 |     | n: 2 |   	 | n: 1 |
;               +------+     +------+     +------+     +------+     +------+     +------+
;               E1           E2           E3           E4           E5           E6
;
;
;
;
;			             +-------> Parameters: n			  +--->	Parameters: product counter max-count
;                                    |         Body: (fact-iter 1 1 n) 	      	  |     Body: (if (> counter max-count) product
;	    		       +---> ()()                                    +--> ()() 	       	  (fact-iter (* counter product) (+ counter 1) max-count))
;	    		       |       |                                     |	    |
;                +-------------+---------------------------------------------+-------------------------------------------------------------------+
;                |             |                                             |                                                                   |
; Global env --> | factorial:--+                                             |                                                                   |
;                | fact-iter:------------------------------------------------+                                                                   |
;                |                                                                                                                               |
;                +-------------------------------------------------------------------------------------------------------------------------------+
; (factorial 6)    ^		    ^		     ^ 	      		^		   ^		      ^			   ^
;                  |		    |		     |	      		|		   |		      |			   |
;                  |		    |  	       	     |	       	       	|      	       	   |   	       	      |	       	       	   |
;               +------+     +------------+     +------------+     +------------+     +------------+     +-------------+     +--------------+
;              	| n: 6 |     | product: 1 |    	| product: 1 | 	   | product: 2 |     | product: 6 |   	 | product: 24 |     | product: 120 |
;               +------+     | counter: 1 |     | counter: 2 |     | counter: 3 |     | counter: 4 |     | counter: 5  |     | counter: 6   |
;                            | max: 6     |     | max: 6     |     | max: 6     |     | max: 6     |     | max: 6      |     | max: 6       |
;                            +------------+     +------------+     +------------+     +------------+     +-------------+     +--------------+
;               E1           E2                 E3                 E4                 E5                 E6                  E7
;
;
;
;
; Exercise 3.10 -------------------------------------------------
;
; Initial state:
;						    +---------> Parameters: initial-amount
; Global env --> +---------------------------+	    |           Body: (let ((balance initial-amount))
;		 | make-withdraw: -----------+----> ()()                (lambda (amount)
;		 |             	             |	      |                  (if (>= balance amount)
;		 |                           |<-------+                    (begin (set! balance (- balance amount))
;		 +---------------------------+                               balance)
;                                                                          "Insufficient funds")))
;

; After evaluating (define W1 (make-withdraw 100))
;
;
; Global env --> +---------------------------+
;		 | make-withdraw: ...        |
;		 |             	             |
;		 | W1:----+                  |
;		 +--------+------------------+
;                         |                ^
;			  |                |
;			  |      	+--+------------------+
;			  +-->()()----->| initial-amount: 100 |<-- E1
;			      |		+---------------------+
;                             |
;			      +----> Parameters: initial-amount
;                                    Body: (if (>= balance amount)
;                                            (begin (set! balance (- balance amount))
;                                              balance)
;                                            "Insufficient funds")
;

; After evaluating (W1 50)
;
; Global env --> +---------------------------+
;		 | make-withdraw: ...        |
;		 |             	             |
;		 | W1:----+                  |
;		 +--------+------------------+
;                         |                ^
;			  |                |
;			  |      	+--+------------------+    +------------+
;			  +-->()()----->| initial-amount: 100 |<-- | amount: 50 |
;			      |		+---------------------+    +------------+
;                             |         E1                         E2
;			      +----> Parameters: initial-amount
;                                    Body: ...


; After evaluating (define W2 (make-withdraw 100))
;                                                      +---------------------+
;                                          +-----------| initial-amount: 100 |
; Global env --> +---------------------------+         +---------------------+
;		 | make-withdraw: ...        |         ^
;		 |             	             |         |    +---> Parameters: 100
;		 | W1:----+      W2:---------+------>()()   |     Body: ...
;		 +--------+------------------+       |      |
;                         |                ^         +------+
;			  |                |
;			  |      	+--+------------------+
;			  +-->()()----->| initial-amount: 100 |
;			      |		+---------------------+
;                             |         E1
;			      +----> Parameters: initial-amount
;                                    Body: ...



; Exercise 3.11 --------------------------------------------------
;
; Global env --> +---------------------------+
;                |                           |<-----+
;		 |                           |	    |
;		 | make-account: ------------+----->()()---> Parameters: balance
;		 |                           |               Body: (define (withdraw amount)
;                +---------------------------+			     (if (>= balance amount)
;									 (begin (set! balance (- balance amount))
;										balance)
;									 "Insufficient funds"))
;		                                                   (define (deposit amount)
;								     (set! balance (+ balance amount))
;								     balance)
;								   (define (dispatch m)
;								     (cond ((eq? m 'withdraw) withdraw)
;									   ((eq? m 'deposit) deposit)
;									   (else (error "Unknown request -- MAKE-ACCOUNT"
;											m))))
;								     dispatch

; After evaluating (define acc (make-account 50))
;
; Global env --> +---------------------------+
;		 |                           |
;		 | make-account: ...         |
;		 | acc: -----+               |
;                +-----------+---------------+
;                            |
;                            +-------> +-------------+
;                                      | balance: 50 |
;                                      +-------------+
