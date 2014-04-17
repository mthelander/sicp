; Section 3.5.1 ------------------------------------------------------------------------------

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; Exercise 3.50 ------------------------------------------------------------------------------

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(stream-map + (stream 1 2 3) (stream 40 50 60) (stream 700 800 900)) ; (741...)

(stream-map (lambda (x y) (+ x (* 2 y)))
	    (stream 1 2 3)
	    (stream 4 5 6)) ; (9...)

; Exercise 3.51 ------------------------------------------------------------------------------

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10))) ; 0
(stream-ref x 5) ; 1 2 3 4 5
(stream-ref x 7) ; 6 7

; Exercise 3.52 ------------------------------------------------------------------------------

(define sum 0) ; sum = 0
(define (accum x)
  (set! sum (+ x sum))
  sum) ; sum = 0
(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; sum = 1
(define y (stream-filter even? seq)) ; sum = 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq)) ; sum = 10

(stream-ref y 7)
; sum = 136
; printed response = 136

(display-stream z)
; sum = 210
; printed response = done

; Yes, because some values would be added to sum multiple times. The memoization ensures
; that accum is only applied to each element exactly once.

; Section 3.5.2 ------------------------------------------------------------------------------

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

; Exercise 3.53 ------------------------------------------------------------------------------

; The stream will contain all the successive powers of two. Proof:

(define s (cons-stream 1 (add-streams s s)))

(define (print-first-n-elements s n)
  (cond ((> n 0)
	 (newline)
	 (display (stream-car s))
	 (print-first-n-elements (stream-cdr s) (- n 1)))))

(print-first-n-elements s 10); 1 2 4 8 16 32 64 128 256 512

; QED

; Exercise 3.54 ------------------------------------------------------------------------------

(define (mul-streams s1 s2)
  (if (stream-null? s1)
      the-empty-stream
      (cons-stream (* (stream-car s1) (stream-car s2))
		   (mul-streams (stream-cdr s1) (stream-cdr s2)))))

(define a (stream 1 3 5 7 11))
(define b (stream 13 17 19 23 27))

(display-stream (mul-streams a b)) ; 13 51 95 161 297

(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(print-first-n-elements factorials 10) ; 1 2 6 24 120 720 5040 40320 362880 3628800

; Exercise 3.55 ------------------------------------------------------------------------------

(define (shift-stream stream offset)
  (stream-map (lambda (x) (+ x offset)) stream))

(define (partial-sums s)
  (let ((first (stream-car s)))
    (cons-stream first
		 (shift-stream (partial-sums (stream-cdr s)) first))))

(print-first-n-elements (partial-sums integers) 5) ; 1 3 6 10 15

; Exercise 3.56 ------------------------------------------------------------------------------

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

(print-first-n-elements S 10) ; 1 2 3 4 5 6 8 9 10 12

; Exercise 3.57 ------------------------------------------------------------------------------

; n additions are performed. If we didn't memoize the repeated computations then it would
; increase to 2^n additions, because computing the nth fibonacci number also requires us to
; compute the (n-1)th number and the (n-2)th number, which would be completely separate
; computations without memoization.

; Exercise 3.58 ------------------------------------------------------------------------------

; The sequence returned by expand contains each digit in the mantissa of the division of
; the given numerator and denominator with the given base. It is essentially performing
; the long division algorithm.

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(print-first-n-elements (expand 1 7 10) 10) ; 1, 4, 2, 8, 5, 7, 1, 4, 2, 8
(print-first-n-elements (expand 3 8 10) 10) ; 3, 7, 5, 0, 0, 0, 0, 0, 0, 0

; Exercise 3.59 ------------------------------------------------------------------------------

; a:

(define (integrate-series series)
  (define (iter-series den series)
    (cons-stream (* (/ 1 den) (stream-car series))
		 (iter-series (+ 1 den) (stream-cdr series))))
  (iter-series 1 series))

(print-first-n-elements (integrate-series integers) 10) ; 1 1 1 1 1 1 1 1 1 1

; b

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(print-first-n-elements cosine-series 6) ; 1 0 -1/2 0 1/24 0
(print-first-n-elements sine-series 6) ; 0 1 0 -1/6 0 1/120

; Exercise 3.60 ------------------------------------------------------------------------------

;; (define (mul-series s1 s2)
;;   (cons-stream (* (stream-car s1) (stream-car s2))
;; 	       (add-streams (mul-series s1 (stream-cdr s2))
;; 			    (mul-series (stream-cdr s1) s2))))

; {a1, a2, a3,...} * {b1, b2, b3,...} = {a1b1, a1b2 + a2b1, a1b3 + a2b2 + a3b1,...}.

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

(print-first-n-elements (add-streams (mul-series sine-series sine-series)
				     (mul-series cosine-series cosine-series)) 5) ; 1 0 0 0 0

; Exercise 3.61 ------------------------------------------------------------------------------

(define (invert-unit-series s)
  (cons-stream (stream-car s)
	       (mul-series (scale-stream (stream-cdr s) -1)
			   (invert-unit-series s))))

(print-first-n-elements (mul-series cosine-series (invert-unit-series cosine-series)) 5) ; 1 0 0 0 0

; Exercise 3.62 ------------------------------------------------------------------------------

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Cannot have a 0 constant in the denominator" s2)
      (mul-series s1 (invert-unit-series s2))))

(define tan-series (div-series sine-series cosine-series))
(print-first-n-elements tan-series 6) ; 0 1 0 1/3 0 2/15

; Section 3.5.3 ------------------------------------------------------------------------------

(define (average . args)
  (/ (apply + args) (length args)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(print-first-n-elements (sqrt-stream 2) 15)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; Exercise 3.63 ------------------------------------------------------------------------------

;; (define (sqrt-stream x)
;;   (cons-stream 1.0
;;                (stream-map (lambda (guess)
;;                              (sqrt-improve guess x))
;;                            (sqrt-stream x))))

; Alyssa is correct because the stream is initialized inside the definition of sqrt-stream,
; therefore reinvoking it will cause it to recompute the stream from the beginning, although
; everything already computed would be memoized. It would be much worse without memo-proc.

; Exercise 3.64 ------------------------------------------------------------------------------

(define (stream-limit stream tolerance)
  (let* ((first (stream-car stream))
	 (rest (stream-cdr stream))
	 (second (stream-car rest)))
    (if (< (abs (- first second)) tolerance)
	second
	(stream-limit rest tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 .25) ; 1.4166666666666665

; Exercise 3.65 ------------------------------------------------------------------------------

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(display (stream-ref (accelerated-sequence euler-transform ln2-stream) 3)) ; .6931488693329254

; These converge really fast.

; Exercise 3.66 ------------------------------------------------------------------------------

(define (print-first-n-elements-with-index s n)
  (define (iter s n i)
    (cond ((> n 0)
	   (newline)
	   (display (stream-car s))
	   (display '=)
	   (display i)
	   (iter (stream-cdr s) (- n 1) (+ i 1)))))
  (iter s n 1))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(print-first-n-elements-with-index (pairs integers integers) 30)

(define (find-pair pair stream)
  (define (iter pair stream num)
    (if (equal? pair (stream-car stream))
	num
	(iter pair (stream-cdr stream) (+ num 1))))
  (iter pair stream 1))

; Every 2nd pair is in the first row
; Every 2^2 pair is in the 2nd row?
; Every 2^3 pair is in the 3rd row?
; Every 2^nth pair is in the bottom row

; First row: 2n-3
; Second row: 3n-2
; Bottom row: 2^n-1

(define (f k n)
  (- (/ (+ (expt 2 k) (expt 2 n)) 2) 1))

(f 4 4) ; 15
(f 7 7) ; 127
(f 2 2) ; 3

(f 2 3) ; 5
(f 2 4) ; 9
(f 2 5) ; 13 NO
(f 4 7) ; 55 NO
(f 6 9) ; 223 NO
(f 1 8) ; 14 NO

; 1(S1 T1) 2(S1 T2) 4(S1 T3) 6(S1 T4)  8(S1 T5)  10(S1 T6) 12(S1 T7)  14(S1 T8)  16(S1 T9) ...
;          3(S2 T2) 5(S2 T3) 9(S2 T4)  13(S2 T5) 17(S2 T6) 21(S2 T7)  25(S2 T8)  29(S2 T9) ...
;                   7(S3 T3) 11(S3 T4) 19(S3 T5) 27(S3 T6) 35(S3 T7)  43(S3 T8)  51(S3 T9) ...
;                            15(S4 T4) 23(S4 T5) 39(S4 T6) 55(S4 T7)  71(S4 T8)  87(S4 T9) ...
;                                      31(S5 T5) 47(S5 T6) 79(S5 T7) 111(S5 T8) 143(S5 T9) ...
;                                                63(S6 T6) 95(S6 T7) 159(S6 T8) 223(S6 T9) ...
;                                                         127(S7 T7) 191(S7 T8) 319(S7 T9) ...
;                                                                    255(S8 T8) 383(S8 T9) ...
;                                                                               511(S9 T9) ...

; 9th column deltas: 128, 64, 96, 80, 56, 36, 22, 13
; 8th column deltas: 64, 32, 48, 40, 28, 18, 11
; 7th column deltas: 32, 16, 24, 20, 14, 9
; 6th column deltas: 16, 8, 12, 10, 7
; 5th column deltas: 8, 4, 6, 5
; 4th column deltas: 4, 2, 3
; 3rd column deltas: 2, 1
; 2nd column deltas: 1

; The symmetrical pairs always have around 2^n-1 preceding pairs (see below).
; The first row has 2n-3 preceding pairs.
; The bottom two rows have ((2^k+2^n)/2)-1 preceding pairs.

; That means that (100 100) has 2^100-1 preceding pairs (a lot!).
; (1 100) has only around 2(100)-3 = 197 preceding pairs.
; (99 100) has ((2^99+2^100)/2)-1 = a lot of preceding pairs.

; Exercise 3.67 ------------------------------------------------------------------------------

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car s)))
		(stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t))))))

(print-first-n-elements (pairs integers integers) 10)
; (1 1) (2 1) (1 2) (3 1) (2 2) (4 1) (1 3) (5 1) (3 2) (6 1)

; Exercise 3.68 ------------------------------------------------------------------------------

; Louis Reasoner is wrong. We need to use cons-stream in our definition, so if we do it his
; way then we get an out of memory exception because it tries to evaluate both streams in
; their entirety.

; Exercise 3.69 ------------------------------------------------------------------------------

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
  		(pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(print-first-n-elements (triples integers integers integers) 10)
; (1 1 1) (1 2 2) (2 2 2) (1 2 3) (2 3 3) (1 3 3) (3 3 3) (1 2 4) (2 3 4) (1 3 4)

(define (is-pythagorean-triple? t)
  (= (+ (expt (car t) 2)
	(expt (cadr t) 2))
     (expt (caddr t) 2)))

(is-pythagorean-triple? (list 3 4 5)) ; #t

(define pythagorean-triples
  (stream-filter is-pythagorean-triple? (triples integers integers integers)))

;(print-first-n-elements pythagorean-triples 4)
; (3 4 5)
; (6 8 10)
; (5 12 13)
; (9 12 15)

; Exercise 3.70 ------------------------------------------------------------------------------

(define (merge-weighted s1 s2 weight)
  (let ((s1car (stream-car s1))
	(s2car (stream-car s2)))
    (cond ((< (weight s1car) (weight s2car))
	   (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
	  ((> (weight s1car) (weight s2car))
	   (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
	  (else
	   (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))))))

(define (pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) w)
    w)))

; a

(define (sum-weight x)
  (apply + x))

(print-first-n-elements (pairs integers integers sum-weight) 10)

; b

(define (more-complicated-weight x)
  (let ((i (car x))
	(j (cadr x)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (filter-by-not-div-by-2-3-5 x)
  (define (not-div-by? n x)
    (not (= 0 (modulo x n))))
  (let ((i (car x))
	(j (cadr x)))
    (and (not-div-by? 2 i) (not-div-by? 3 i) (not-div-by? 5 i)
	 (not-div-by? 2 j) (not-div-by? 3 j) (not-div-by? 5 j))))

(print-first-n-elements
 (stream-filter filter-by-not-div-by-2-3-5 (pairs integers integers more-complicated-weight))
 10)

; Exercise 3.71 ------------------------------------------------------------------------------

(define (take stream n)
  (if (= n 0) '()
      (cons (stream-car stream)
	    (take (stream-cdr stream)
		  (- n 1)))))

(define (after stream n)
  (if (= n 0) stream
      (after (stream-cdr stream) (- n 1))))

(define (find-consecutive-weights stream weight num-weights)
  (let ((weights (map weight (take stream num-weights)))
	(rest (stream-cdr stream)))
    (if (apply = weights)
	(cons-stream (take stream num-weights)
		     (find-consecutive-weights rest weight num-weights))
	(find-consecutive-weights rest weight num-weights))))

(define (ramanujan-numbers)
  (define (weight pair)
    (+ (expt (car pair) 3) (expt (cadr pair) 3)))
  (stream-map (lambda (x) (weight (car x)))
	      (find-consecutive-weights (pairs integers integers weight) weight 2)))

(print-first-n-elements (ramanujan-numbers) 6)

; 1729
; 4104
; 13832
; 20683
; 32832
; 39312

;; ; Exercise 3.72 ------------------------------------------------------------------------------

(define (sum-of-2-squares)
  (define (weight pair)
    (+ (expt (car pair) 2) (expt (cadr pair) 2)))
  (find-consecutive-weights (pairs integers integers weight) weight 3))

(print-first-n-elements (sum-of-2-squares) 5)

; 325 = ((1 18) (6 17) (10 15))
; 425 = ((5 20) (88 19) (13 16))
; 650 = ((5 25) (11 23) (17 19))
; 725 = ((7 26) (10 25) (14 23))
; 845 = ((2 29) (13 26) (19 22))

; Exercise 3.73 ------------------------------------------------------------------------------

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
		 (integral (scale-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))
(take (RC1 integers 3) 5) ; (8 13.5 19.5 26 33)

; Exercise 3.74 ------------------------------------------------------------------------------

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 0 0 0))

(define (sign-change-detector before after)
  (cond ((< before 0 after) 1)
	((> before 0 after) -1)
	(else 0)))

; Alyssa P. Hacker's code

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(print-first-n-elements zero-crossings 12)
; 0  0    0  0    0     -1  0   0   0     0    1  0  0

; Eva Lu Ator's code

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(print-first-n-elements zero-crossings 12)

; Exercise 3.75 ------------------------------------------------------------------------------

; Louis Reasoner's code calculates incorrectly average the average values. The solution
; is to modify make-zero-crossings to keep track of the last average value and compare it
; to the current average.

(define (make-zero-crossings input-stream last-value last-avg)
  (let* ((curr (stream-car input-stream))
	 (avpt (/ (+ last-value curr) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
				      curr
				      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

(print-first-n-elements zero-crossings 12)
; 0  0    0  0    0     -1  0   0   0     0    1  0  0

; Exercise 3.76 ------------------------------------------------------------------------------
 
(define (smooth stream)
  (let ((rest (stream-cdr stream)))
    (cons-stream (/ (+ (stream-car stream) (stream-car rest)) 2)
		 (smooth rest))))

(define (make-zero-crossings input-stream smooth-proc)
  (define (iter stream last-value)
    (let ((curr (stream-car stream)))
      (cons-stream (sign-change-detector curr last-value)
		   (iter (stream-cdr stream) curr))))
  (iter (smooth-proc input-stream) 0))

(define zero-crossings (make-zero-crossings sense-data smooth))

(print-first-n-elements zero-crossings 12)

; Exercise 3.77 ------------------------------------------------------------------------------

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
                             dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ; 2.716923932235896

; Exercise 3.78 ------------------------------------------------------------------------------

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b)
			   (scale-stream dy a)))
  y)

(define 2nd-deriv-test (solve-2nd 2 3 5 8 13))
(take 2nd-deriv-test 5)

; Exercise 3.79 ------------------------------------------------------------------------------

(define (solve-2nd-generic f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f y dy))
  y)

(define 2nd-derive-test-generic (solve-2nd-generic * 3 5 8))
(take 2nd-derive-test-generic 5)

; Exercise 3.80 ------------------------------------------------------------------------------

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (* -1 (/ 1 C))))
    (define dil (add-streams (scale-stream vc (/ 1 L))
			     (scale-stream il (* -1 R))))
    (stream-map cons vc il)))

(define rlc-circuit-test (RLC 1 1 0.2 0.1))
(take (rlc-circuit-test 10 0) 5);  ((10 0) (10 1) (9.5 1.9) (8.55 2.66) (7.220000000000001 3.249))

; Exercise 3.81 ------------------------------------------------------------------------------

(define (random-number-generator requests)
  (define random-init (random 100))
  (define (rand-update x)
    (let ((a 6364136223846793005)
	  (b 1442695040888963407)
	  (m (expt 2 64)))
      (modulo (+ (* a x) b) m)))
  (define random-numbers
    (cons-stream random-init
		 (stream-map rand-update random-numbers)))
  (define (successor current-request nums)
    (case current-request
      ((generate) (stream-cdr nums))
      ((reset) random-numbers)
      (else (error "Invalid request -- " current-request))))
  (define (generate-numbers reqs nums)
    (if (stream-null? reqs)
  	the-empty-stream
  	(let ((current-request (stream-car reqs)))
	  (cons-stream (stream-car nums)
		       (generate-numbers (stream-cdr reqs)
					 (successor current-request nums))))))
  (generate-numbers requests random-numbers))

(define test-requests '(generate generate generate reset generate generate reset generate))

(take (random-number-generator (apply stream test-requests)) (length test-requests))

; Exercise 3.82 ------------------------------------------------------------------------------

(define (estimate-integral P x1 x2 y1 y2)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))

  (define (random-points)
    (cons-stream (list (random-in-range x1 x2)
		       (random-in-range y1 y2))
		 (random-points)))

  (define request-stream (stream-map (lambda (point) (apply P point))
				     (random-points)))

  (let ((deltas (* (- x1 x2) (- y1 y2)))
	(sqr-radius (expt 3 2)))
    (stream-map (lambda (pass-ratio) (/ (* deltas pass-ratio) sqr-radius))
  	      (monte-carlo request-stream 0 0))))

(define (is-in-circle? x y)
  (let ((center-x 5)
	(center-y 7)
	(radius 3))
    (<= (+ (expt (- x center-x) 2)
	   (expt (- y center-y) 2))
	(expt radius 2))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi-estimate (estimate-integral is-in-circle? 2.0 8.0 4.0 10.0))

(stream-ref pi-estimate 1000)

;(stream-ref (accelerated-sequence euler-transform pi-estimate) 1000)
