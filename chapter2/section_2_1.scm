; Section 2.1.1 -----------------------------------------------------------------

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; Exercise 2.1 -------------------------------------------------------------------

(define (make-rat n d)
  (define (xor a b)
    (and (or a b)
	 (not (and a b))))
  (let ((num (abs n))
	(den (abs d)))
    (let ((g (gcd num den)))
      (if (xor (< n 0) (< d 0))
	  (cons (- (/ num g)) (/ den g))
	  (cons (/ num g) (/ den g))))))

(print-rat (make-rat 4 -3)) ; -4/3
(print-rat (make-rat -4 3)) ; -4/3
(print-rat (make-rat -2 -9)) ; 2/9
(print-rat (make-rat 2 9)) ; 2/9

; Exercise 2.2 -------------------------------------------------------------------

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
		    (x-point (end-segment segment)))
		 2)
	      (/ (+ (y-point (start-segment segment))
		    (y-point (end-segment segment)))
		 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define test-segment (make-segment (make-point 3 5)
				   (make-point 14 12)))

(print-point (midpoint-segment test-segment))
;; (17/2,17/2)

; Exercise 2.3 -------------------------------------------------------------------

(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))

(define (bottom-left rect) (car rect))
(define (top-right rect) (cdr rect))

(define (width rect)
  (- (x-point (top-right rect))
     (x-point (bottom-left rect))))
(define (height rect)
  (- (y-point (top-right rect))
     (y-point (bottom-left rect))))

(define (area rect)
  (* (width rect)
     (height rect)))

(define (perimeter rect)
  (+ (* 2 (height rect))
     (* 2 (width rect))))

(area (make-rectangle (make-point 0 0) (make-point 7 4))) ; 28
(perimeter (make-rectangle (make-point 0 0) (make-point 7 4))) ; 22

(define (make-rectangle width height) (cons width height))
(define (width rect) (car rect))
(define (height rect) (cdr rect))

(area (make-rectangle 7 4)) ; 28
(perimeter (make-rectangle 7 4)) ; 22


; Exercise 2.4 -------------------------------------------------------------------

;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; (car (cons 3 56)) ; 3
;; (car (cons '(asdf) 5)) ; (asdf)

;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; (cdr (cons 3 56)) ; 56
;; (cdr (cons 3 '((foo) bar))) ; ((foo) bar)

; Using the substitution model:

; (cdr (cons 3 56))
; (cdr (lambda (m) (m 3 56)))
; ((lambda (m) (m 3 56)) (lambda (p q) q))
; ((lambda (p q) q) 3 56)
; 56

; Exercise 2.5 -------------------------------------------------------------------

;; (define (cons a b)
;;   (* (expt 2 a)
;;      (expt 3 b)))

;; (define (count-divisions num n)
;;   (define (iter p c)
;;     (if (= (modulo p n) 0)
;; 	(iter (/ p n) (+ c 1))
;; 	c))
;;   (iter num 0))

;; (define (car p)
;;   (count-divisions p 2))

;; (define (cdr p)
;;   (count-divisions p 3))

;; (car (cons 3 7)) ; 3
;; (cdr (cons 3 7)) ; 7

; Exercise 2.6 -------------------------------------------------------------------

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;; Using substitution for (add-1 zero):
;; (lambda (f)
;;   (lambda (x)
;;     (f ((zero f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) x)) x)))))
;; (lambda (f)
;;   (lambda (x)
;;     (f x))) <-- This will be one.

(define one (lambda (f)
	      (lambda (x)
		(f x))))

;; Now lets try this again by evaluating (add-1 one):
;; (lambda (f)
;;   (lambda (x)
;;     (f ((one f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (((lambda (x) (f x))) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f (f x)))) <-- This will be two.

(define two (lambda (f)
	      (lambda (x)
		(f (f x)))))

(define (plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; Section 2.1.1 -----------------------------------------------------------------

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

; Exercise 2.7 -------------------------------------------------------------------

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(add-interval (make-interval 3 5) (make-interval 8 9)) ; (11 . 14)

; Exercise 2.8 -------------------------------------------------------------------

(define (sub-interval x y)
  (add-interval x (make-interval (- (lower-bound y))
				 (- (upper-bound y)))))

(sub-interval (make-interval 5 6) (make-interval 7 9)) ; (-2 . -3)

; Exercise 2.9 -------------------------------------------------------------------

(define (width-interval x)
  (/ (- (lower-bound x) (upper-bound x)) 2))

(width-interval (make-interval 2 4)) ; -1
(width-interval (make-interval 4 8)) ; -2

(width-interval (add-interval (make-interval 2 4)
			      (make-interval 4 8))) ; -3
(width-interval (mul-interval (make-interval 2 4)
			      (make-interval 4 8))) ; -12

(width-interval (make-interval 5 10)) ; -5/2
(width-interval (make-interval 15 20)) ; -5/2

(width-interval (add-interval (make-interval 5 10)
			      (make-interval 15 20))) ; -5
(width-interval (mul-interval (make-interval 5 10)
			      (make-interval 15 20))) ; -125

; The width of the sum appears to simply be the sum of the width of its addends.
; The width of the product doesn't appear to be the sum/product/division.

; Exercise 2.10 ------------------------------------------------------------------

(define (interval-spans-n? x n)
  (and (< (lower-bound x) n)
       (> (upper-bound x) n)))

(define (div-interval x y)
  (if (interval-spans-n? y 0)
      (error "Cannot divide by an interval that spans zero!" y))
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; (div-interval (make-interval 5 10)
;; 	      (make-interval -2 5))
;; ; Cannot divide by an interval that spans zero! (-2 . 5)

; Exercise 2.11 ------------------------------------------------------------------

(define (mul-interval x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (cond ((and (negative? lx) (negative? ux) (negative? ly) (negative? uy))
	   (make-interval (* lx ly) (* ux uy)))
	  ((and (negative? lx) (negative? ux) (negative? ly) (positive? uy))
	   (make-interval (* lx uy) (* lx ly)))
	  ((and (negative? lx) (negative? ux) (positive? ly) (positive? uy))
	   (make-interval (* lx uy) (* ly uy)))
	  ((and (negative? lx) (positive? ux) (positive? ly) (positive? uy))
	   (make-interval (* lx ly) (* ux uy)))
	  ((and (positive? lx) (positive? ux) (positive? ly) (positive? uy))
	   (make-interval (* lx ly) (* ux uy)))
	  ((and (positive? lx) (positive? ux) (negative? ly) (negative? uy))
	   (make-interval (* ux ly) (* lx uy)))
	  ((and (positive? lx) (positive? ux) (negative? ly) (positive? uy))
	   (make-interval (* ux ly) (* ux uy)))
	  ((and (negative? lx) (positive? ux) (negative? ly) (negative? uy))
	   (make-interval (* ux ly) (* lx ly)))
	  ((and (negative? lx) (positive? ux) (negative? ly) (positive? uy))
	   (make-interval (min (* ux ly) (* lx uy))
			  (max (* lx ly) (* ux uy)))))))

(mul-interval (make-interval 2 4) (make-interval -2 4)) ; (-8 . 16)

; Exercise 2.12 ------------------------------------------------------------------

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent ctr pct)
  (make-center-width ctr (* ctr (/ pct 100.0))))

(define (percent int)
  (* 100.0
     (/ (width int)
	(center int))))

(make-center-percent 56 12.7) ; (48.888 . 63.112)
(percent (make-center-percent 56 12.7)) ; 12.700000000000003
