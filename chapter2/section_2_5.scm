;; Exercise 2.77 -----------------------------------------------------------------
;; Louis Reasoner tries to evaluate the expression (magnitude z) where z is the object shown in figure 2.24. To his surprise, instead of the answer 5 he gets an error message from apply-generic, saying there is no method for the operation magnitude on the types (complex). He shows this interaction to Alyssa P. Hacker, who says ``The problem is that the complex-number selectors were never defined for complex numbers, just for polar and rectangular numbers. All you have to do to make this work is add the following to the complex package:''

;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)

;; Describe in detail why this works. As an example, trace through all the procedures called in evaluating the expression (magnitude z) where z is the object shown in figure 2.24. In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?

(define put 2d-put!)
(define (get x-key y-key)
  (let ((1d-table (2d-get-alist-x x-key)))
    (let ((type-f (assoc y-key 1d-table)))
      (if type-f (cdr type-f) false))))

; From 2.4

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; end 2.4

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; Suggested fix
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(magnitude (make-complex-from-real-imag 3 4))

; This works because the complex-number package reuses the implementation of
; rectangular numbers. A call to magnitude evaluates like this:

; (magnitude '(complex rectangular 3 4))
; (apply-generic 'magnitude '(complex rectangular 3 4))
; (apply (get 'magnitude '(complex)) '(rectangular 3 4))
; (apply-generic 'magnitude '(rectangular 3 4))
; (apply (get 'magnitude '(rectangular)) (3 4))
; ...

; So apply-generic gets invoked twice; the first one is dispatched on the complex
; package, then the second on the rectangular package.



;; Exercise 2.78. ----------------------------------------------------------------
;; The internal procedures in the scheme-number package are essentially nothing more than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language directly because our type-tag system requires that each data object have a type attached to it. In fact, however, all Lisp implementations do have a type system, which they use internally. Primitive predicates such as symbol? and number? determine whether data objects have particular types. Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2 so that our generic system takes advantage of Scheme's internal type system. That is to say, the system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (and (pair? datum) (symbol? (car datum)))
      (car datum)
      'scheme-number))

(define (contents datum)
  (if (and (pair? datum) (symbol? (car datum)))
      (cdr datum)
      datum))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(apply-generic 'add
	       (make-scheme-number '3)
	       (make-scheme-number '4)) ; 7
(apply-generic 'add 3 4) ; 7

; Exercise 2.79 ------------------------------------------------------------------
; Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

;; ; I feel like this should work, but it's kind of cheating
;; (define (install-equ?-package)
;;   (define (equ? x y)
;;     (equal? x y))
;;   (put 'equ? '(scheme-number scheme-number) equ?)
;;   (put 'equ? '(rational-number rational-number) equ?)
;;   (put 'equ? '(complex complex) equ?))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define _pre-equ?-install-complex-package install-complex-package)
(define (install-complex-package)
  (_pre-equ?-install-complex-package)
  (define (equ? x y)
     (and (= (real-part x) (real-part y))
	 (= (imag-part x) (imag-part y))
	 (= (magnitude x) (magnitude y))
	 (= (angle x) (angle y))))
  (put 'equ? '(complex complex) equ?)
  'done)

(define _pre-equ?-install-scheme-number-package install-scheme-number-package)
(define (install-scheme-number-package)
  (_pre-equ?-install-scheme-number-package)
  (define (equ? x y)
    (= (contents x) (contents y)))
  (put 'equ? '(scheme-number scheme-number) equ?)
  'done)

(define _pre-equ?-install-rational-package install-rational-package)
(define (install-rational-package)
  (_pre-equ?-install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (put 'equ? '(rational rational) equ?)
  'done)

(define (equ? x y)
  (apply-generic 'equ? x y))

(install-complex-package)
(install-scheme-number-package)
(install-rational-package)

;(make-complex-from-real-imag 3 4)
(equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4)) ; #t
(equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 5)) ; #f
(equ? (make-complex-from-mag-ang 3 4) (make-complex-from-mag-ang 3 4)) ; #t
(equ? (make-scheme-number 7) (make-scheme-number 3)) ; #f
(equ? (make-scheme-number 3) (make-scheme-number 3)) ; #t
(equ? (make-rational 7 3) (make-rational 7 3)) ; #t


; Exercise 2.80 ------------------------------------------------------------------
; Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

(define _pre-install-=zero?-complex-package install-complex-package)
(define (install-complex-package)
  (_pre-install-=zero?-complex-package)
  (define (=zero? x)
    (and (= (real-part x) 0)
	 (= (imag-part x) 0)
	 (= (magnitude x) 0)
	 (= (angle x) 0)))
  (put '=zero? '(complex) =zero?)
  'done)

(define _pre-install-=zero?-scheme-number-package install-scheme-number-package)
(define (install-scheme-number-package)
  (_pre-install-=zero?-scheme-number-package)
  (define (=zero? x)
    (= x 0))
  (put '=zero? '(scheme-number) =zero?)
  'done)

(define _pre-install-=zero?-rational-package install-rational-package)
(define (install-rational-package)
  (_pre-install-=zero?-rational-package)
  (define (numer x) (car x))
  (define (=zero? x)
    (= (numer x) 0))
  (put '=zero? '(rational) =zero?)
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))

(install-complex-package)
(install-scheme-number-package)
(install-rational-package)

(=zero? (make-complex-from-real-imag 0 0)) ; #t
(=zero? (make-complex-from-mag-ang 0 0)) ; #t
(=zero? (make-scheme-number 0)) ; #t
(=zero? (make-scheme-number 3)) ; #f
(=zero? (make-rational 0 3)) ; #t
(=zero? (make-rational 1 2)) ; #f
(=zero? (make-rational 3 0)) ; #f

; Exercise 2.81 ------------------------------------------------------------------

; a) It would recur infinitely,

; b) Louis was not correct. apply-generic counts on the fact that it will
; if we cannot coerce the arguments then it will terminate.

; c) Modify apply-generic so that it doesn't try coercion if the two arguments have the same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
		   (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; (define (exp x y) (apply-generic 'exp x y))

;; (put 'exp '(scheme-number scheme-number)
;;      (lambda (x y) (tag (expt x y))))

;; (exp (make-complex-from-real-imag 3 4)
;;      (make-complex-from-real-imag 8 4)) ; No method for these types (exp (complex complex))

; Exercise 2.82 ------------------------------------------------------------------
; Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)

(define *coercion-table* (make-equal-hash-table))

(define (put-coercion type-from type-to proc)
  (hash-table/put! *coercion-table* (list type-from type-to) proc))

(define (get-coercion type-from type-to)
  (hash-table/get *coercion-table* (list type-from type-to) #f))

(define (all-the-same? l)
  (true-for-all? l eq?))

(define (true-for-all? l proc)
  (if (< (length l) 2)
      true
      (and (proc (car l) (cadr l))
	   (true-for-all? (cdr l) proc))))

(all-the-same? '(foo bar baz)) ; #f
(all-the-same? '(foo foo foo)) ; #t
(all-the-same? '(foo)) ; #t
(all-the-same? '()) ; #t
(all-the-same? '(foo foo foo foo bar)) ; #f

(define (can-coerce-all? master-type rest)
  (or (null? rest)
      (and (or (eq? master-type (car rest))
	       (get-coercion (car rest) master-type))
	   (can-coerce-all? master-type (cdr rest)))))

(put-coercion 'bar 'foo (lambda (x) 'foo))
(put-coercion 'baz 'foo (lambda (x) 'foo))
(put-coercion 'bak 'foo (lambda (x) 'foo))

(can-coerce-all? 'foo '(foo bar baz bak)) ; #t
(can-coerce-all? 'bar '(bar baz bak)) ; #f

(define p pretty-print)

(define (coerce-to master-type elem)
  (if (eq? master-type (type-tag elem))
      elem
      (let ((proc (get-coercion (type-tag elem) master-type)))
	(if proc
	    (proc (contents elem))
	    (error "no coercion method defined for these types"
		   (list master-type (type-tag elem)))))))

(define (get-first-proc-that-can-coerce-all args)
  (define (iter-over-candidates master-candidates)
    (if (null? args)
	false
	(let ((master-type (car master-candidates)))
	  (if (can-coerce-all? master-type args)
	      (lambda (x) (coerce-to master-type x))
	      (iter-over-candidates (cdr master-candidates))))))
  (iter-over-candidates args))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (all-the-same? type-tags))
	      (let ((coercion-proc (get-first-proc-that-can-coerce-all type-tags)))
		(if coercion-proc
		    (apply apply-generic op (map coercion-proc args))
		    (error "No method for these types" (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

(put-coercion 'scheme-number 'rational (lambda (x)
					 (make-rational (contents x) 1)))

(add (make-rational 3 1)
     (make-scheme-number 2)) ; # (rational 5 1)

; This will only work if every arguments' type maps directly to another in the
; list. If there's one that needed, say, 2 steps to be coerced then it would
; not work.

; Exercise 2.83 ------------------------------------------------------------------
; Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).

(define *tower* '(scheme-number rational real complex))

(define (raise x)
  (define (iter-over l)
    (cond ((null? (cdr l)) '())
	  ((eq? (car l) (type-tag x))
	   (coerce-to (cadr l) x))
	  (else (iter-over (cdr l)))))
  (iter-over *tower*))

(raise (make-scheme-number 3)) ; # (rational 3 1)

; Exercise 2.84 ------------------------------------------------------------------
; Using the raise operation of exercise 2.83, modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising, as discussed in this section. You will need to devise a way to test which of two types is higher in the tower. Do this in a manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels to the tower.

(define (make-real x)
  (attach-tag 'real x))

(put-coercion 'real 'rational
	      (lambda (x) (make-rational (contents x) 1)))
(put-coercion 'rational 'real
	      (lambda (x)
		(define (numer x) (car x))
		(define (denom x) (cdr x))
		(make-real (/ (numer x) (denom x)))))

(define (is-higher? x y)
  (define (iter-tower l)
    (let ((z (car l)))
      (cond ((eq? x z) false)
	    ((eq? y z) true)
	    (else (iter-tower (cdr l))))))
  (iter-tower *tower*))

(is-higher? 'rational 'real) ; #f
(is-higher? 'complex 'real) ; #t

(define (find-highest-type types)
  (define (iter-tower highest types)
    (if (null? types)
	highest
	(let ((x (car types)))
	  (if (is-higher? x highest)
	      (iter-tower x (cdr types))
	      (iter-tower highest (cdr types))))))
  (iter-tower (car types) (cdr types)))

(find-highest-type '(real rational complex)) ; complex

(define (raise-to-type type x)
  (if (eq? type (type-tag x))
      x
      (raise-to-type type (raise x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (all-the-same? type-tags))
	      (let ((highest-type (find-highest-type type-tags)))
		(apply apply-generic op
		       (map (lambda (x)
			      (raise-to-type highest-type x))
			    args)))
              (error "No method for these types"
                     (list op type-tags)))))))

(raise (make-rational 3 1)) ; (real 3)

(add (make-rational 3 1)
     (make-scheme-number 2)) ; # (rational 5 1)

; Exercise 2.85 ------------------------------------------------------------------
; This section mentioned a method for ``simplifying'' a data object by lowering it in the tower of types as far as possible. Design a procedure drop that accomplishes this for the tower described in exercise 2.83. The key is to decide, in some general way, whether an object can be lowered. For example, the complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all. Here is a plan for determining whether an object can be lowered: Begin by defining a generic operation project that ``pushes'' an object down in the tower. For example, projecting a complex number would involve throwing away the imaginary part. Then a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with. Show how to implement this idea in detail, by writing a drop procedure that drops an object as far as possible. You will need to design the various projection operations53 and install project as a generic operation in the system. You will also need to make use of a generic equality predicate, such as described in exercise 2.79. Finally, use drop to rewrite apply-generic from exercise 2.84 so that it ``simplifies'' its answers.

(define (install-project)
  (define (project-complex x)
    (make-real (real-part x)))
  (define (project-real x)
    (make-rational (contents x) 1))
  (define (project-rational x)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (make-scheme-number (round (/ (numer x) (denom x)))))
  (put 'project '(complex) project-complex)
  (put 'project '(real) project-real)
  (put 'project '(rational) project-rational))

(define (drop x)
  (let ((projected-value (project x)))
    ;(p projected-value)
    (if (equ? (raise projected-value) x)
	(drop projected-value)
	projected-value)))

(define (project x)
  (apply-generic 'project x))

(install-project)

(project (make-complex-from-real-imag 3 4)) ; (real 3)
(project (make-real 5)) ; (rational 5 1)
(project (make-rational 7 3)) ; 2

;(drop (make-complex-from-real-imag 1 0)) ; (real 1)

; INCOMPLETE

; Exercise 2.86

; INCOMPLETE

; Section 2.53

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;<procedures same-variable? and variable? from section 2.3.2>
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
    ;; representation of terms and term lists
    ;<procedures adjoin-term ...coeff from text below>
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; Exercise 2.87 ------------------------------------------------------------------------------
; Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.

(install-polynomial-package)

(define (install-=zero?-polynomials)
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (poly=zero? poly)
    (terms=zero? (term-list poly)))

  (define (terms=zero? terms)
    (if (null? terms)
	true
	(and (=zero? (coeff (first-term terms)))
	     (terms=zero? (rest-terms terms)))))

  (put '=zero? '(polynomial) poly=zero?))

(install-=zero?-polynomials)

; polynomial: 8x^3 + 2x^2 + 5x + 3
(define *test-poly* (make-polynomial 'x '((3 8) (2 2) (1 5) (0 3))))

(=zero? *test-poly*) ; #f
(=zero? (make-polynomial 'x '((2 0) (1 0) (0 0)))) ; #t

(make-polynomial 'y '((1 1) (0 1))) ; y + 1
(make-polynomial 'x '((2 (make-polynomial y ((1 1) (0 1)) 0)) (0 1))) ; (y + 1)^2 + 1
(make-polynomial 'x '((100 1) (2 2) (0 1))) ; x^100 2x^2 + 1

(add (make-polynomial 'x '((2 1) (1 3) (0 5)))
     (make-polynomial 'x '((2 4) (1 2) (0 9)))) ; (polynomial x (2 5) (1 5) (0 14))

(define (prettify-polynomial polynomial)
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (let ((poly (contents polynomial)))
    (let ((var (variable poly)))
      (define (print-term term)
	(let ((coefficient (coeff term)))
	  (if (is-polynomial? coefficient)
	      (begin (display #\() (prettify-polynomial coefficient) (display #\)))
	      (if (not (eq? 1 coefficient))
		   (display coefficient)))
	  (if (eq? (order term) 0)
	      (display var)
	      (map display (list var "^" (order term))))))

      (define (is-polynomial? coefficient)
	(pair? coefficient))

      (define (iter-terms terms)
	(if (not (null? terms))
	    (let ((current (first-term terms))
		  (rest (rest-terms terms)))
	      (print-term current)
	      (if (not (null? rest)) (display "+"))
	      (iter-terms rest))))
      (iter-terms (term-list poly)))))

(define *nested-poly1*
  (make-polynomial 'x
		   (list
		    (list 2 (make-polynomial 'y (list (list 1 1) (list 0 1))))
		    (list 1 (make-polynomial 'y (list (list 2 2) (list 0 1))))
		    (list 0 (make-polynomial 'y (list (list 1 1) (list 0 -1)))))))

(define *nested-poly2*
  (make-polynomial 'x
		   (list
		    (list 1 (make-polynomial 'y (list (list 1 1) (list 0 -2))))
		    (list 0 (make-polynomial 'y (list (list 3 1) (list 0 7)))))))

(prettify-polynomial *nested-poly1*) ; (1y^1+1y^0)x^2+(2y^2+1y^0)x^1+(1y^1+-1y^0)x^0
(prettify-polynomial *nested-poly2*) ; (1y^1+-2y^0)x^1+(1y^3+7y^0)x^0
(prettify-polynomial (add *nested-poly1* *nested-poly2*)) ; (1y^1+1y^0)x^2+(2y^2+1y^1+-1y^0)x^1+(1y^3+1y^1+6y^0)x^0
(prettify-polynomial (mul *nested-poly1* *nested-poly2*)) ; (1y^2+-1y^1+-2y^0)x^3+(1y^4+3y^3+-4y^2+8y^1+5y^0)x^2+(2y^5+1y^3+15y^2+-3y^1+9y^0)x^1+(1y^4+-1y^3+7y^1+-7y^0)x^0

; Exercise 2.88 ------------------------------------------------------------------------------
; Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful to define a generic negation operation.)

(define (install-subtract)
  (define (variable p) (car p))
  (define (subtract-polynomial p1 p2)
    (add (attach-tag 'polynomial p1)
  	 (mul (attach-tag 'polynomial p2)
  	      (make-polynomial (variable p2) '((0 -1))))))
  (put 'sub '(polynomial polynomial) subtract-polynomial)
  'done)

(install-subtract)

(sub (make-polynomial 'x '((2 5) (1 6) (0 9)))
     (make-polynomial 'x '((2 3) (1 2) (0 8)))) ; (polynomial x (2 2) (1 4) (0 1))

; Exercise 2.89 ------------------------------------------------------------------------------
; Define procedures that implement the term-list representation described above as appropriate for dense polynomials.

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term other-term)
    (let ((x (order term))
	  (y (order other-term)))
      (cond ((= x (+ y 1))
	     (cons (coeff term) other-term))
	    ((> x y)
	     (adjoin-term term (cons 0 other-term)))
	    (else (error "Cannot adjoin a smaller order term to a larger one"
			 (list term other-term))))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term ordr coeff)
    (define (iter n accum)
      (if (eq? 0 n) (cons coeff accum)
	  (iter (- n 1)
		(cons 0 accum))))
    (iter ordr '()))
  
  (define (order term-list)
    (- (length term-list) 1))

  (define (coeff term-list)
    (first-term term-list))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
  	(make-poly (variable p1)
  		   (add-terms (term-list p1)
  			      (term-list p2)))
  	(error "Polys not in same var -- ADD-POLY"
  	       (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))

  (define (pad-left-with-zeros l n)
    (if (eq? (length l) n) l
	(pad-left-with-zeros (cons 0 l) n)))

  (define (add-terms l1 l2)
    (let ((size (max (length l1) (length l2))))
      (define (iter-terms terms1 terms2 accum)
	(if (or (empty-termlist? terms1) (empty-termlist? terms2)) accum
	    (iter-terms (rest-terms terms1)
			(rest-terms terms2)
			(cons (add (first-term terms1)
				   (first-term terms2))
			      accum))))
				     
      (reverse (iter-terms (pad-left-with-zeros l1 size)
			   (pad-left-with-zeros l2 size)
			   (the-empty-termlist)))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms L1 L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms l1 l2)
    (if (empty-termlist? l2)
	(the-empty-termlist)
	(adjoin-term (make-term (+ (order l1) (order l2))
				(mul (coeff l1) (coeff l2)))
		     (mul-term-by-all-terms l1 (rest-terms l2)))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(add (make-polynomial 'x '(1 2 0 3 -2 -5))
     (make-polynomial 'x '(3 5 0 6  9 -2))) ; (polynomial 'x '(4 7 0 9 7 -7))

(add (make-polynomial 'x '(1 2 0 3 -2 -5))
     (make-polynomial 'x '(3))) ; (polynomial 'x '(1 2 0 3 -2 -2))

(mul (make-polynomial 'x '(3 5 0 6 9 -2))
     (make-polynomial 'x '(5))) ; (polynomial 'x '(15 25 0 30 45 -10))

(mul (make-polynomial 'x '(3 5 0 6  9 -2))
     (make-polynomial 'x '(3 5 0 6  9 -2))) ; (polynomial 'x '(9 15 0 27 -6))

; Exercise 2.90 ------------------------------------------------------------------------------
; Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.

(define (install-polynomial-sparse-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (tag p) (attach-tag 'sparse p))

  (put 'add '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-polynomial-dense-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term other-term)
    (let ((x (order term))
	  (y (order other-term)))
      (cond ((= x (+ y 1))
	     (cons (coeff term) other-term))
	    ((> x y)
	     (adjoin-term term (cons 0 other-term)))
	    (else (error "Cannot adjoin a smaller order term to a larger one"
			 (list term other-term))))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term ordr coeff)
    (define (iter n accum)
      (if (eq? 0 n) (cons coeff accum)
	  (iter (- n 1)
		(cons 0 accum))))
    (iter ordr '()))
  
  (define (order term-list)
    (- (length term-list) 1))

  (define (coeff term-list)
    (first-term term-list))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
  	(make-poly (variable p1)
  		   (add-terms (term-list p1)
  			      (term-list p2)))
  	(error "Polys not in same var -- ADD-POLY"
  	       (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))

  (define (pad-left-with-zeros l n)
    (if (eq? (length l) n) l
	(pad-left-with-zeros (cons 0 l) n)))

  (define (add-terms l1 l2)
    (let ((size (max (length l1) (length l2))))
      (define (iter-terms terms1 terms2 accum)
	(if (or (empty-termlist? terms1) (empty-termlist? terms2)) accum
	    (iter-terms (rest-terms terms1)
			(rest-terms terms2)
			(cons (add (first-term terms1)
				   (first-term terms2))
			      accum))))
				     
      (reverse (iter-terms (pad-left-with-zeros l1 size)
			   (pad-left-with-zeros l2 size)
			   (the-empty-termlist)))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms L1 L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms l1 l2)
    (if (empty-termlist? l2)
	(the-empty-termlist)
	(adjoin-term (make-term (+ (order l1) (order l2))
				(mul (coeff l1) (coeff l2)))
		     (mul-term-by-all-terms l1 (rest-terms l2)))))

  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-polynomial-package)
  (install-polynomial-sparse-package)
  (install-polynomial-dense-package)

  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (apply-generic 'add p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (apply-generic 'mul p1 p2))))
  (put 'make 'polynomial
       (lambda (type var terms)
	 (tag ((get 'make type) var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial) 'sparse var terms))

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial) 'dense var terms))

(make-polynomial-sparse 'x '((3 4) (2 8) (1 9) (0 3)))

(make-polynomial-sparse 'x '(4 8 9 3))

(add (make-polynomial-dense 'x '(1 2 0 3 -2 -5))
     (make-polynomial-dense 'x '(3 5 0 6  9 -2))) ; (polynomial dense x (4 7 0 9 7 -7))

(add (make-polynomial-dense 'x '(1 2 0 3 -2 -5))
     (make-polynomial-dense 'x '(3))) ; (polynomial x (1 2 0 3 -2 -2))

(mul (make-polynomial-dense 'x '(3 5 0 6 9 -2))
     (make-polynomial-dense 'x '(5))) ; (polynomial x (15 25 0 30 45 -10))

(mul (make-polynomial-dense 'x '(3 5 0 6  9 -2))
     (make-polynomial-dense 'x '(3 5 0 6  9 -2))) ; (polynomial x (9 30 25 36 114 78 16 108 -36 4))

; Exercise 2.91 ------------------------------------------------------------------------------

(define (install-polynomial-sparse-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  ; new code =================================================================================

  (define (sub-terms l1 l2)
    (add-terms l1 (mul-terms l2 '((0 -1)))))

  (define (div-terms dividend divisor)
    (if (empty-termlist? dividend)
	(list (the-empty-termlist) (the-empty-termlist))
	(let ((t1 (first-term dividend))
	      (t2 (first-term divisor)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-termlist) dividend)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let* ((result (make-term new-o new-c))
		       (rest-of-result
			(div-terms (sub-terms dividend (mul-terms (list result) divisor))
				   divisor)))
		  (list (adjoin-term result (car rest-of-result))
			(cadr rest-of-result))))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
  	(div-terms (term-list p1) (term-list p2))
  	(error "Cannot divide polynomials of different terms" (list p1 p2))))

  (put 'div '(sparse sparse)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  ; ==========================================================================================

  (define (tag p) (attach-tag 'sparse p))

  (put 'add '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-polynomial-dense-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term other-term)
    (let ((x (order term))
	  (y (order other-term)))
      (cond ((= x (+ y 1))
	     (cons (coeff term) other-term))
	    ((> x y)
	     (adjoin-term term (cons 0 other-term)))
	    (else (error "Cannot adjoin a smaller order term to a larger one"
			 (list term other-term))))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term ordr coeff)
    (define (iter n accum)
      (if (eq? 0 n) (cons coeff accum)
	  (iter (- n 1)
		(cons 0 accum))))
    (iter ordr '()))
  
  (define (order term-list)
    (- (length term-list) 1))

  (define (coeff term-list)
    (first-term term-list))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
  	(make-poly (variable p1)
  		   (add-terms (term-list p1)
  			      (term-list p2)))
  	(error "Polys not in same var -- ADD-POLY"
  	       (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))

  (define (pad-left-with-zeros l n)
    (if (eq? (length l) n) l
	(pad-left-with-zeros (cons 0 l) n)))

  (define (add-terms l1 l2)
    (let ((size (max (length l1) (length l2))))
      (define (iter-terms terms1 terms2 accum)
	(if (or (empty-termlist? terms1) (empty-termlist? terms2)) accum
	    (iter-terms (rest-terms terms1)
			(rest-terms terms2)
			(cons (add (first-term terms1)
				   (first-term terms2))
			      accum))))
				     
      (reverse (iter-terms (pad-left-with-zeros l1 size)
			   (pad-left-with-zeros l2 size)
			   (the-empty-termlist)))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms L1 L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms l1 l2)
    (if (empty-termlist? l2)
	(the-empty-termlist)
	(adjoin-term (make-term (+ (order l1) (order l2))
				(mul (coeff l1) (coeff l2)))
		     (mul-term-by-all-terms l1 (rest-terms l2)))))

  ; new code =================================================================================

  ; We need this to eliminate leading zeros, otherwise a polynomial
  ; is never reduced in size and div-terms never completes
  (define (simplify terms)
    (cond ((empty-termlist? terms) (the-empty-termlist))
	  ((eq? (first-term terms) 0) (simplify (rest-terms terms)))
	  (else terms)))

  (define (sub-terms l1 l2)
    (add-terms l1 (mul-terms l2 '(-1))))

  (define (div-terms dividend divisor #!optional depth)
    (if (empty-termlist? dividend)
	(list (the-empty-termlist) (the-empty-termlist))
	(if (> (order divisor) (order dividend))
	    (list (the-empty-termlist) dividend)
	    (let ((new-c (div (coeff dividend) (coeff divisor)))
		  (new-o (- (order dividend) (order divisor))))
	      (let* ((result (make-term new-o new-c))
	      	     (rest-of-result
	      	      (div-terms (simplify (sub-terms dividend (mul-terms result divisor)))
	      			 divisor)))
	      	(list (adjoin-term result (car rest-of-result))
	      	      (cadr rest-of-result)))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
  	(div-terms (term-list p1) (term-list p2) 0)
  	(error "Cannot divide polynomials of different terms" (list p1 p2))))

  (put 'div '(dense dense)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  ; ==========================================================================================

  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-polynomial-package)
  (install-polynomial-sparse-package)
  (install-polynomial-dense-package)

  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (apply-generic 'add p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (apply-generic 'mul p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (apply-generic 'div p1 p2))))
  (put 'make 'polynomial
       (lambda (type var terms)
	 (tag ((get 'make type) var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial) 'sparse var terms))

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial) 'dense var terms))

(div (make-polynomial-sparse 'x '((5 1) (0 -1)))
     (make-polynomial-sparse 'x '((2 1) (0 -1))))

(div (make-polynomial-dense 'x '(1 0 0 0 0 -1))
     (make-polynomial-dense 'x '(1 0 -1)))
