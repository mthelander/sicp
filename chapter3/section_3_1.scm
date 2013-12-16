; Exercise 3.1 ------------------------------------------------------------------------------

(define (make-accumulator accum)
  (lambda (n)
    (set! accum (+ accum n))
    accum))
  
(define A (make-accumulator 5))

(A 10); 15
(A 10); 25

; Exercise 3.2 ------------------------------------------------------------------------------

(define (make-monitored f)
  (let ((num-calls 0))
    (lambda (n)
      (case n
	((how-many-calls?) num-calls)
	((reset-count) (set! num-calls 0))
	(else (set! num-calls (+ 1 num-calls))
	      (f n))))))

(define s (make-monitored sqrt))

(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 'reset-count)
(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 144) ; 12
(s 'how-many-calls?) ; 2

; Exercise 3.3 ------------------------------------------------------------------------------

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password-message . x) "Incorrect password")
  (define (dispatch p m)
    (cond ((not (eq? p password)) incorrect-password-message)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

; Exercise 3.4 ------------------------------------------------------------------------------

(define (make-account balance password)
  (let ((num-incorrect-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password-message . x) "Incorrect password")
    (define (call-the-cops) (pretty-print "Called the cops"))
    (define (dispatch p m)
      (cond ((not (eq? p password))
	     (set! num-incorrect-attempts (+ 1 num-incorrect-attempts))
	     (if (> num-incorrect-attempts 7) (call-the-cops))
	     incorrect-password-message)
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

; Exercise 3.5 ------------------------------------------------------------------------------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let* ((predicate (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2))))
	 (proportion-in-circle (monte-carlo trials predicate))
	 (radius 3)
	 (area (* (- x1 x2) (- y1 y2) proportion-in-circle)))
    (/ area (expt radius 2))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (is-in-circle? x y)
  (let ((center-x 5)
	(center-y 7)
	(radius 3))
    (<= (+ (expt (- x center-x) 2)
	   (expt (- y center-y) 2))
	(expt radius 2))))

(estimate-integral is-in-circle? 2.0 8.0 4.0 10.0 1000) ; 3.14416

; Exercise 3.6 ------------------------------------------------------------------------------

(define random-init (random 100))

(define rand
  (let ((x random-init))
    (lambda (arg)
      (cond ((eq? arg 'generate)
	     (set! x (random x))
	     x)
	    ((eq? arg 'reset)
	     (lambda (new-val)
	       (set! x new-val)
	       x))
	    (else (error "Invalid argument") s)))))

(rand 'generate)
((rand 'reset) 15)
(rand 'generate)

; Exercise 3.7 ------------------------------------------------------------------------------

(define (make-joint account password new-password)
  (define (incorrect-password-message . x) "Incorrect password")
  (let ((result ((account password 'deposit) 0)))
    (if (number? result)
	(lambda (p m)
	  (if (eq? p new-password)
	      (account password m)
	      incorrect-password-message))
	(error "Invalid password for account -- " password))))

(define peter-acc (make-account 34 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'deposit) 55) ; 89
((paul-acc 'rosebud 'deposit) 144) ; 233

; Exercise 3.8 ------------------------------------------------------------------------------

(define f
  (let ((prev 0))
    (lambda (n)
      (let ((temp prev))
	(set! prev n)
	temp))))

(+ (f 0) (f 1)) ; 1
(+ (f 1) (f 0)) ; 0
