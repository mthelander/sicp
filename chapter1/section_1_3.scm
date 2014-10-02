; Section 1.3.1 ------------------------------------------------------------------

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

; Exercise 1.29 ------------------------------------------------------------------

(define (simpsons-rule-integration f a b n)
  (let ((h (/ (- b a) n)))
    (define (coefficient k)
      (cond ((= k 0) 1)
    	    ((= k n) 1)
    	    ((even? k) 2)
    	    (else 4)))
    (define (term k)
      (* (coefficient k)
	 (f (+ a (* k h)))))
    (* (/ h 3)
       (sum term 0 inc n))))

(integral cube 0 1 0.01) ; .24998750000000042
(simpsons-rule-integration cube 0 1 100) ; 1/4

(integral cube 0 1 0.001) ; .249999875000001
(simpsons-rule-integration cube 0 1 1000) ; 1/4

; Exercise 1.30 ------------------------------------------------------------------

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
        (iter (next a)
	      (+ (term a) result))))
  (iter a 0))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000)) ; 3.139592655589782

; Exercise 1.31 ------------------------------------------------------------------

(define (identity x) x)

; a.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
        (iter (next a)
	      (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 6) ; 720
(factorial 15) ; 1307674368000

(define (approximate-pi accuracy)
  (define (evens k)
    (if (even? k) k
	(evens (+ k 1))))
  (define (odds k)
    (+ (evens k) 1))
  (* 4 (/ (product evens 2 inc (+ accuracy 1))
	  (product odds  1 inc accuracy))))

(approximate-pi 6) ; 4096/1224 = 3.3436...

; b.

(define (product term a next b)
  (if (> a b) 1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 6) ; 720
(factorial 15) ; 1307674368000

; Exercise 1.32 ------------------------------------------------------------------

; a.

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(factorial 6) ; 720
(* 8 (pi-sum 1 1000)) ; 3.139592655589783

; b.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
        (iter (next a)
	      (combiner (term a) result))))
  (iter a null-value))

(factorial 6) ; 720
(* 8 (pi-sum 1 1000)) ; 3.139592655589783

; Exercise 1.33 ------------------------------------------------------------------

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))


(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
	((filter a)
	 (combiner (term a)
		   (filtered-accumulate combiner null-value term (next a) next b filter)))
	(else
	 (filtered-accumulate combiner null-value term (next a) next b filter))))

; a.

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-squares-of-primes 1 100) ; 65797

; b.

(define (product-of-rel-primes n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc n rel-prime?))

(product-of-rel-primes 20) ; 8729721
(product-of-rel-primes 10) ; 189

; Section 1.3.2 ------------------------------------------------------------------

; Exercise 1.34 ------------------------------------------------------------------

; It will die with "The object 2 is not applicable". This happens because inside
; the first invocation of f, it calls (f 2), but f expects to be passed a
; procedure instead of a constant value.

; Section 1.3.3 ------------------------------------------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 1.35 ------------------------------------------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; 1.6180327868852458

; Exercise 1.36 ------------------------------------------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) ; 4.555532270803653

; 34 steps without average damping

(define (average x y)
  (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0) ; 4.555537551999825

; Only 9 steps with average damping!

; Exercise 1.37 ------------------------------------------------------------------

; a.

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)	0
	(/ (n i)
	   (+ (d i) (iter (+ i 1))))))
  (iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           13) ; .6180371352785146

; 13 seems to be the magic number to get 4 decimal digits of accuracy.

; b.

(define (cont-frac n d k)
  (define (iter i accum)
    (if (= i 0)
	accum
	(iter (- i 1)
	      (/ (n i)
		 (+ (d i) accum)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           13) ; .6180371352785146

; Exercise 1.38 ------------------------------------------------------------------

(define (estimate-e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
		  (lambda (i) 
		    (if (divides? 3 (+ i 1))
			(/ (* 2 (+ i 1)) 3)
			1))
		  k)))

(estimate-e 5) ; 2.71875

; Exercise 1.39 ------------------------------------------------------------------

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)	1
	(/ (n i)
	   (+ (d i) (iter (+ i 1))))))
  (iter 1))


(define (tan-cf x k)
  (cont-frac (lambda (i)
	       (if (= i 1) x
		   (* -1 (square x))))
	     (lambda (i)
	       (+ (* 2 (- i 1)) 1))
	     k))

(tan-cf 5 3) ; 35/157

; Section 1.3.4 ------------------------------------------------------------------

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; Exercise 1.40 ------------------------------------------------------------------

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 3 -2.4 6) 1) ; -3.9813366488302706

; Exercise 1.41 ------------------------------------------------------------------

(define (double proc)
  (lambda (x)
    (proc (proc x))))

((double inc) 7) ; 9
(((double (double double)) inc) 5) ; 21

; Exercise 1.42 ------------------------------------------------------------------

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6) ; 49

; Exercise 1.43 ------------------------------------------------------------------

(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ; 625

; Exercise 1.44 ------------------------------------------------------------------

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

((smooth square) 8) ; 64.00000000006666

(define (n-fold-smooth n)
  (repeated smooth n))

(((n-fold-smooth 9) square) 8) ; 64.00000000060001

; Exercise 1.45 ------------------------------------------------------------------

(define (nth-root n)
  (lambda (x)
    (fixed-point
     ((repeated average-damp (/ (log n) (log 2)))
      (lambda (y) (/ x (expt y (- n 1)))))
     1.0)))

((nth-root 2) 16) ; 4.000000000000051

((nth-root 4) 256) ; 4.0000000000000006

((nth-root 16) (expt 8 16)) ; 8.

; Exercise 1.46 ------------------------------------------------------------------

(define (iterative-improve good-enough-proc improve-proc)
  (lambda (guess)
    (let ((next (improve-proc guess)))
      (if (good-enough-proc guess next)
	  guess
	  ((iterative-improve good-enough-proc improve-proc) next)))))

(define (sqrt x)
  (define (good-enough? guess n) ; ?
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 16) ; 4.000000636692939

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; 1.6180371352785146
