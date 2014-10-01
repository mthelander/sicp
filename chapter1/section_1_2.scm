; Exercise 1.9 ---------------------------------------------------------------------

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
; This is a recursive process.

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
;
; This is an iterative process.

; Exercise 1.10 --------------------------------------------------------------------

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; (A 0 (A 0 (A 1 14)))
;; (A 0 (A 0 (A 0 (A 1 13))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10))))))) ; We know from the previous question that (A 1 10) = 1024
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
;; (A 0 (A 0 (A 0 (A 0 4096))))
;; (A 0 (A 0 (A 0 8192)))
;; (A 0 (A 0 16384))
;; (A 0 32768)
;; 65536

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 (A 0 2))
;; (A 2 4) ; Use answer from previous question
;; 65536

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; (f n) computes 2n
; (g n) computes 2^n
; (h n) computes 2^^n (tetration with base of 2)

; Exercise 1.11 --------------------------------------------------------------------
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

; Recursive process

; f(0) -> 0
; f(1) -> 1
; f(2) -> 2
; f(3) -> f(2) + 2*f(1) + 3*f(0) -> 2 + 2*1 + 3*0 -> 4
; f(4) -> f(3) + 2*f(2) + 3*f(1) -> 4 + 2*2 + 3*1 -> 11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(f 1) ; 1
(f 2) ; 2
(f 3) ; 4
(f 4) ; 11
(f 12) ; 10661
(f 20) ; 10771211

(define (f n)
  (define (f-iter a b c count)
    (if (= count 0)
	a
	(f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))

(f 1) ; 1
(f 2) ; 2
(f 3) ; 4
(f 4) ; 11
(f 12) ; 10661
(f 20) ; 10771211

; Exercise 1.12 --------------------------------------------------------------------

;; The following pattern of numbers is called Pascal's triangle.
;;
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;;
;; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(define (build-next-row prev-row)
  (define (inner-els els)
    (if (eq? (length els) 1)
	'()
	(cons (+ (car els) (cadr els))
	      (inner-els (cdr els)))))
  (append (list 1) (inner-els prev-row) (list 1)))

(define (peek l)
  (cond ((null? l) '())
	((eq? (length l) 1) (car l))
	(else (peek (cdr l)))))

(define (push el l)
  (append l (list el)))

(define (pascals-triangle n)
  (if (= n 1)
      (list (list 1))
      (let ((subtriangle (pascals-triangle (- n 1))))
	(push (build-next-row (peek subtriangle)) subtriangle))))

; Just for fun, pretty print this
(define (print-spaces n)
  (cond ((> n 1)
	 (print-spaces (- n 1))
	 (display " "))))
(define (print-chars l)
  (cond ((not (null? l))
	 (display (car l))
	 (display " ")
	 (print-chars (cdr l)))))
(define (prettify-pascals-triangle triangle)
  (if (not (null? triangle))
      (let ((depth (length (peek triangle)))
	    (current (car triangle)))
	(print-spaces (+ (- depth (length current)) 1))
	(print-chars current)
	(display "\n")
	(prettify-pascals-triangle (cdr triangle)))))
(define p prettify-pascals-triangle)

(p (pascals-triangle 1)) ; ((1))
(p (pascals-triangle 2)) ; ((1) (1 1))
(p (pascals-triangle 3)) ; ((1) (1 1) (1 2 1))
(p (pascals-triangle 4)) ; ((1) (1 1) (1 2 1) (1 3 3 1))
(p (pascals-triangle 5)) ; ((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))
(p (pascals-triangle 6)) ; ((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1) (1 5 10 10 5 1)

;(print-chars '(x y z))

; Exercise 1.13

;; Prove that Fib(n) is the closest integer to phi^n/sqrt(5), where phi = (1 + sqrt(5))/2. Hint: Let psi = (1 - sqrt(5))/2. Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (phi^n - psi^n)/sqrt(5).

; f(0) = 0
; f(1) = 1
; f(n) = f(n-1) + f(n-2)
; 
; f(0) = 0
; f(n) = f(n-1) + f(n-2)
; f(n+1) = f(n+1-1) + f(n+1-2) = f(n) + f(n-1)

; ...

; SKIP

; Exercise 1.14

; LOST. SKIP.

; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a.

; 5 times.

; b.

; It's O(logn) in both space and time.

; Exercise 1.16

(define (fast-iterative-expt b n)
  (fast-iterative-expt-iter b n 1))

(define (fast-iterative-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n)
	 (fast-iterative-expt-iter (square b) (/ n 2) a))
	(else
	 (fast-iterative-expt-iter b (- n 1) (* a b)))))

(fast-iterative-expt 2 8) ; 256
(fast-iterative-expt 8 3) ; 512
(fast-iterative-expt 5 13) ; 1220703125

; Exercise 1.17

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
	((even? b)
	 (double (fast-* a (halve b))))
	(else
	 (+ a (fast-* a (- b 1))))))

(fast-* 8 9) ; 72
(fast-* 5 7) ; 35
(fast-* 593 1145) ; 678985

; Exercise 1.18

(define (fast-iterative-* a b)
  (fast-iterative-*-iter a b 0))

(define (fast-iterative-*-iter a b acc)
  (cond ((= b 0) acc)
	((even? b)
	 (fast-iterative-*-iter (double a) (halve b) acc))
	(else
	 (fast-iterative-*-iter a (- b 1) (+ a acc)))))

(fast-iterative-* 8 9) ; 72
(fast-iterative-* 5 7) ; 35
(fast-iterative-* 593 1145) ; 678985

; Exercise 1.19

;; Let's use the b transformation since it's shorter:

;;    T(b) = bp+aq
;;   2T(b) = T(b)*p+T(a)*q
;;         = (bp+aq)*p+(bq+aq+ap)*q
;;         = (bp^2+aqp)+(bq^2+aq^2+apq)
;; bp'+aq' = b*(p^2+q^2)+a*(2pq+q^2)

;; We've converted the right side into the same form as the left side,
;; therefore it follows that p'=p^2+q^2, and q'=2pq+q^2.

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
		   (+ (square p) (square q))
		   (+ (* 2 q p) (square q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))

(map (lambda (n) (fib n))
     (list 0 1 2 3 4 5 6 7 8 9 10))
;; (0 1 1 2 3 5 8 13 21 34 55)

; Exercise 1.20

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

;; Applicative order:

;; (gcd 206 40)
;; (= 40 0)
;; (gcd 40 (remainder 206 40))
;; (= 6 0)
;; (gcd 6 (remainder 40 6))
;; (= 4 0)
;; (gcd 4 (remainder 6 4))
;; (= 2 0)
;; (gcd 2 (remainder 4 2))
;; (= 0 0)

;; 4 remainder applications.

;; Normal order:

;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	40
;; 	(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	40
;; 	(if (= (remainder 40 (remainder 206 40)) 0)
;; 	    (remainder 206 40)
;; 	    (gcd (remainder 40 (remainder 206 40))
;; 		 (remainder (remainder 206 40)
;; 			    (remainder 40 (remainder 206 40)))))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;; 	40
;; 	(if (= (remainder 40 (remainder 206 40)) 0)
;; 	    (remainder 206 40)
;; 	    (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;; 		(remainder 40 (remainder 206 40))
;; 		(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; 		     (remainder (remainder 40 (remainder 206 40))
;; 				(remainder (remainder 206 40)
;; 					   (remainder 40 (remainder 206 40)))))))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0) ; 1
;; 	40
;; 	(if (= (remainder 40 (remainder 206 40)) 0) ; 2
;; 	    (remainder 206 40)
;; 	    (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ; 4
;; 		(remainder 40 (remainder 206 40))
;; 		(if (= (remainder (remainder 40 (remainder 206 40))
;; 				  (remainder (remainder 206 40)
;; 					     (remainder 40 (remainder 206 40)))) ; 7
;; 		       0)
;; 		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; 4
;; 		    ...)))))

;; 18 invocations!

; Exercise 1.21

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

(map smallest-divisor (list 199 1999 19999)) ; (1 99 1999 7)

; Exercise 1.22

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
    (cond ((even? start)
	   (search-for-primes (+ start 1) end))
	  ((<= start end)
	   (timed-prime-test start)
	   (search-for-primes (+ start 2) end))))

(search-for-primes 1000 1020)
;; 1009 *** 0.
;; 1013 *** 0.
;; 1019 *** 0.

(search-for-primes 10000 10100)
;; 10007 *** 0.
;; 10009 *** 0.
;; 10037 *** 0.

(search-for-primes 100000 100100)
;; 100003 *** 0.
;; 100019 *** 0.
;; 100043 *** 0.

(search-for-primes 1000000 1000200)
;; 1000003 *** 9.999999999999898e-3
;; 1000033 *** 0.
;; 1000037 *** 0.

;; These results don't tell us much, it appears that the timer's precision isn't high enough.

; Exercise 1.23

(define (next n)
  (if (= 2 n) 3 (+ 2 n)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(for-each timed-prime-test
	  (list 1009    1013    1019
		10007   10009   10037
		100003  100019  100043
		1000003 1000033 1000037))

; Again, we need a more granular time function to be able to answer this.


; Exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) 
                       start-time))))

(for-each timed-prime-test
	  (list 1009    1013    1019
		10007   10009   10037
		100003  100019  100043
		1000003 1000033 1000037))

;; Once again, we can't answer this conclusively without a more accurate runtime
;; procedure. However, I would speculate that it would be orders of magnitude
;; faster than the O(sqrt n) method. O(1000000) = 1000, Log2(1000000) ~ 19.
;; Any discrepancies that occur may be caused by the times parameter to
;; fast-prime?.

; Exercise 1.25

; Yes, we could use her way. However, the algorithm given prevents the need for
; exceeding the modulo value given by exploiting the "x times y modulo m" trick
; described in footnote 46. It's just a helpful optimization.

; Exercise 1.26

; It's become an O(n) process because at the recursive call he's doubled the
; number of computations. The call tree splits at every even number, so it's
; essentially halving the number of calculations then doubling it. (n/2)*2=n.

; Exercise 1.27

(define (is-congruent? n)
  (define (iter-from a)
    (cond ((>= a n) true)
	  ((= (expmod a n n) a)
	   (iter-from (+ a 1)))
	  (else false)))
  (iter-from 2))

(is-congruent? 8) ; #f
(is-congruent? 17) ; #t

(map is-congruent? (list 561 1105 1729 2465 2821 6601)) ; (#t #t #t #t #t #t)

; Exercise 1.28

(define (expmod base exp m)
  (define (non-trivial-sqrt? a)
    (and (not (= a 1))
  	 (not (= a (- m 1)))
  	 (= (remainder (square a) m) 1)))
  (define (maybe-square a)
    (if (non-trivial-sqrt? a) 0
  	(square a)))
  (cond ((= exp 0) 1)
        ((even? exp)
	 (remainder (maybe-square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 17 5) ; true
(fast-prime? 8 5) ; false

(map (lambda (n) (fast-prime? n 10))
     (list 561 1105 1729 2465 2821 6601)) ; (#f #f #f #f #f #f)
