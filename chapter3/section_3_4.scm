; Exercise 3.38 ------------------------------------------------------------------------------

; a)

; 1:
;; (set! balance (+ balance 10))
;; (set! balance (- balance 20))
;; (set! balance (- balance (/ balance 2)))
;; Balance: $45

; 2:
;; (set! balance (- balance 20))
;; (set! balance (- balance (/ balance 2)))
;; (set! balance (+ balance 10))
;; Balance: $50

; 1.
;; (set! balance (- balance (/ balance 2)))
;; (set! balance (+ balance 10))
;; (set! balance (- balance 20))
;; Balance: $40

; a)


    ;;                  +------+
    ;;    +-------------+ $100 +---------------+
    ;;    |             +------+               |
    ;;    |                |                   |
    ;;    |                |                   |
    ;; +-------+           |               +---+----+
    ;; | 100/2 |           |               | 100-20 |
    ;; +--+----+       +---+----+          +---+----+
    ;;    |            | 100+10 |              |
    ;;    |            +---+----+              |
    ;;    |                |                   |
    ;;    |            +---+--+                |
    ;;    |            | $110 |                |
    ;;    |            +------+                |
    ;;    |                                    |
    ;;    +----------> +------+                |
    ;;                 | $50  |                |
    ;;                 +------+                |
    ;;                                         |
    ;;                 +------+ <--------------+
    ;;                 | $80  |
    ;;                 +------+


; Section 3.4.2

; Exercise 3.39 ------------------------------------------------------------------------------

;; 101: P1 sets x to 100 and then P2 increments x to 101.
;; 121: P2 increments x to 11 and then P1 sets x to x times x.
;; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

; Exercise 3.40 ------------------------------------------------------------------------------

;; (define x 10)

;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (* x x x))))

; Without serialized procs:

; 1_000_000: P1 squares x, then P2 cubes x, OR: P2 cubes x, then P1 squares bx.
; 100_000:   P2 accesses x, then P1 sets x to 100, then P2 accesses x twice more, then sets x to 100_000.
; 10_000:    P2 accesses x twice, then P1 sets x to 100, then P2 accesses x and sets x to 10_000.
; 1_000:     P1 accesses x twice, then P2 accesses x three times, then P1 sets x to 100, then P2 sets x to 1_000.
; 100:       P2 accesses x three times, then P1 twice, then P2 sets x to 1_000, then P1 sets x to 100.

; With serialized procs:
; 1_000_000: P1 squares x, then P2 cubes x, OR: P2 cubes x, then P1 squares bx.

; Exercise 3.41 ------------------------------------------------------------------------------

; Yes, because the balance returned could not be trusted to be accurate.

;; (define acc (make-account 100))
;; (parallel-execute (lambda () ((acc 'deposit) 50))
;; 		  (lambda () ((acc 'withdraw) (/ (acc 'balance) 2))))

; Actually, I don't think using a serialized balance procedure would change this fact.

; Exercise 3.42 ------------------------------------------------------------------------------

; Yes, it is safe to make that change. There is no difference.

; Exercise 3.43 ------------------------------------------------------------------------------

;; (define (exchange account1 account2)
;;   (let ((difference (- (account1 'balance)
;;                        (account2 'balance))))
;;     ((account1 'withdraw) difference)
;;     ((account2 'deposit) difference)))

; If the processes are run sequentially, then no matter how many exchanges are performed the 3 original balances will remain in the set of account balances, since it's simply a rearrangement of elements in the set.

; $10          $20           $30
; |            /  \           |
; balance1: 10     balance2: 20
; balance2: 20     balance3: 30
; delta:   -10     delta:   -10
; |                |
; set! balance1    |
; to 10--10=20     set! balance2
; |                to 20--10=30
; set balance2     |
; to 30+-10=20     set balance3
; |          |     to 30+-10=20
; |          |     |
; $10       $20    $20

; Because the two processes' access to the accounts are intertwined, the account balances
; can change in between when the delta is calculated and when the new value is set.
; Because the same amount is first added then subtracted from two different accounts, the
; sum of all three accounts will never change so long as each process completed successfully.

; However, if the individual account accesses are allowed to interleave completely, then
; we can't guarantee that the set! operations will be atomic.

; Using the previous diagram, if the two set! operations on account 2 are performed at the
; same time, we end up with the following scenario:

; ((account2 'deposit) -10)         ((account2 'withdraw) -10)
;            |                                   |
; (set! balance (+ balance -10))    (set! balance (- balance -10))
;            |                                   |
; (set! balance (+ 20 -10))         (set! balance (- 20 -10))
;            |                                   |
;            |                           balance = 20
;    balance = 10

; So our final account balances would be $10, $10, and $10 -- not good!

; Exercise 3.44 ------------------------------------------------------------------------------

; No, Louis is wrong. The serialization in the exchange procedure is necessary because
; the balances of each account are accessed twice, with the first access influencing the
; final withdrawal/deposit. The transfer procedure on the other hand just dithdraws/deposits
; a constant value into both accounts, making it safe to remain unserialized.

; Exercise 3.45 ------------------------------------------------------------------------------

; We'll end up with a deadlock condition, because the withdraw and deposit procedures will get
; serialized twice.

; Exercise 3.46 ------------------------------------------------------------------------------

;; (define (make-mutex)
;;   (let ((cell (list false)))            
;;     (define (the-mutex m)
;;       (cond ((eq? m 'acquire)
;;              (if (test-and-set! cell)
;;                  (the-mutex 'acquire))) ; retry
;;             ((eq? m 'release) (clear! cell))))
;;     the-mutex))
;; (define (clear! cell)
;;   (set-car! cell false))

;; (define (test-and-set! cell)
;;   (if (car cell)
;;       true
;;       (begin (set-car! cell true)
;;              false)))

; (the-mutex m)          (the-mutex m)
;       |                      |
; (set-car! cell true)   (set-car! cell true)
;       |                      |
; (if (car cell) ...)    (if (car cell) ...)
;       |                      |
; (if false ...)         (if false ...)
;       |                      |  
; (set-car! cell true)   (set-car! cell true)

; Because both procedures called test-and-set! at the same time, they both got past the
; if statement and attempted to set the cell, thus returning false.

; Exercise 3.47 ------------------------------------------------------------------------------

; In terms of mutexes

(define (make-semaphore n)
  (let ((mutex (make-mutex))
	(num-acquired n))
    (define (dec)
      (set! num-acquired (- n 1)))
    (define (inc)
      (set! num-acquired (+ n 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (> num-acquired 0)
		 (dec)
		 (if (eq? num-acquired 0) (mutex m))))
	    ((eq? m 'release)
	     (if (< num-acquired n)
		 (inc)
		 (mutex m)))))
    the-semaphore))

; In terms of atomic test-and-set! operations

(define (make-semaphore n)
  (let ((cell (list 0)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell n)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-semaphore))
(define (clear! cell)
  (if (> (car cell) 0)
      (set-car! cell (- (car cell) 1))))

(define (test-and-set! cell max)
  (if (eq? (car cell) max)
      true
      (begin (set-car! cell (+ 1 (car cell)))
             false)))


; Exercise 3.48 ------------------------------------------------------------------------------

; This enforced ordering works because it ensures that a set of accounts is always accessed
; in the same order by each process. For instance, if account (x, y) are accessed, then we
; are guaranteed to never access (y, x).

(define (make-account-and-serializer balance account-num)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'account-num) account-num)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (if (> (account1 'account-num) (account2 'account-num))
      (serialized-exchange account2 account1)
      (let ((serializer1 (account1 'serializer))
	    (serializer2 (account2 'serializer)))
	((serializer1 (serializer2 exchange))
	 account1
	 account2))))

; Exercise 3.49 ------------------------------------------------------------------------------

; The described deadlock-avoidance mechanism works fine if all accounts are known in advance,
; but if the accounts to access can only be determined by another account access then it will
; not work. For instance, if we have bank accounts A and B, and we wish to make a withdrawal
; from A, and we have a requirement that if A doesn't have a sufficient balance then instead
; do the withdrawal from B. We can't know whether or not we need to access B until we've already
; accessed A, so the deadlock-avoidance mechanism could fail.
