; Section 1.3.1 ------------------------------------------------------------------

(define (cube x) (* x x x))

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
    (define (inc k)
      (+ k 1))
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

