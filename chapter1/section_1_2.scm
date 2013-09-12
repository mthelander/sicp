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

