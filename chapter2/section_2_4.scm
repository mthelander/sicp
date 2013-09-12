(define p pretty-print)

(define get 2d-get)
(define put 2d-put!)

; Exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; a.  Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?

; The deriv procedure was modified to use a lookup table to do a data-directed
; dispatch on the operator.
; We can't assimilate the number? and variable? predicates because there isn't
; an operator to dispatch on in those cases.

; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (left exp) (car exp))
(define (right exp) (cadr exp))

(define (install-product-package)
  (define (deriv-product exp var)
    (make-sum
     (make-product (left exp)
		   (deriv (right exp) var))
     (make-product (deriv (left exp) var)
		   (right exp))))
  (put 'deriv '* deriv-product))

(define (install-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (left exp) var)
	      (deriv (right exp) var)))
  (put 'deriv '+ deriv-sum))

(install-sum-package)
(install-product-package)

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

; c. Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and install it in this data-directed system.

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e1 1) e1)
	((and (number? e1) (number? e2)) (expt e1 e2))
	(else (list '** e1 e2))))

(define (install-exponentiation-package)
  (define (deriv-exponent exp var)
    (let ((base (left exp)) (exponent (right exp)))
      (make-product
       (make-product exponent
		     (make-exponentiation base (- exponent 1)))
       (deriv base var))))
  (put 'deriv '** deriv-exponent))

(install-exponentiation-package)

(deriv '(** x 3) 'x) ; (* 3 (** x 2))

; d. In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like
; ((get (operator exp) 'deriv) (operands exp) var)
; What corresponding changes to the derivative system are required?

; We would just need to switch the order when we call put to install our packages.

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))

(define (install-product-package)
  (define (deriv-product exp var)
    (make-sum
     (make-product (left exp)
		   (deriv (right exp) var))
     (make-product (deriv (left exp) var)
		   (right exp))))
  (put '* 'deriv deriv-product))

(define (install-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (left exp) var)
	      (deriv (right exp) var)))
  (put '+ 'deriv deriv-sum))

(install-sum-package)
(install-product-package)

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

; Exercise 2.74

;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

;; Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

;; a. Implement for headquarters a get-record procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type information must be supplied?

(define (install-example-division-package)
  (define (personnel-records)
    '((emp1 1 3) (emp2 5 44) (emp3 2 22)))

  (define (employee-name record) (car record))
  (define (employee-salary record) (caddr record))

  (put 'personnel-records 'example personnel-records)
  (put 'employee-name 'example employee-name)
  (put 'employee-salary 'example employee-salary))

(install-example-division-package)

(define (get-personnel-records division)
  ((get 'personnel-records division)))

(define (get-employee-name division record)
  ((get 'employee-name division) record))

(define (get-record division name)
  (define (get-record-by-name records)
    (if (null? records)
	'()
	(let ((rec (car records)))
	  (if (eq? (get-employee-name division rec) name)
	      rec
	      (get-record-by-name (cdr records))))))
  (get-record-by-name (get-personnel-records division)))

(get-record 'example 'emp3) ; (emp3 2 22)

; In this implementation, divisions define their own structure for personnel records, they just need to provide the selector procedures for each field.

;; b. Implement for headquarters a get-salary procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work?

(define (get-employee-salary division record)
  ((get 'employee-salary division) record))

(define (get-salary division name)
  (let ((record (get-record division name)))
    (if (null? record)
	'()
	(get-employee-salary division record))))

(get-salary 'example 'emp3) ; 22
(get-salary 'example 'emp2) ; 44

; As long as each division provides their own selectors for each field, then it can be structured in any way.

;; c. Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files.

(define (install-another-division-package)
  (define (personnel-records)
    '((emp23 5 55) (emp24 4 666)))

  (define (employee-name record) (car record))
  (define (employee-salary record) (caddr record))

  (put 'personnel-records 'another personnel-records)
  (put 'employee-name 'another employee-name)
  (put 'employee-salary 'another employee-salary))

(install-another-division-package)

(define (find-employee-record divisions name)
  (if (null? divisions)
      '()
      (let ((record (get-record (car divisions) name)))
  	(if (null? record)
  	    (find-employee-record (cdr divisions) name)
  	    record))))

(find-employee-record '(example another) 'emp24)

;; d.  When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

; The new company will just need to provider their own selectors that define the structure of a record.

; Exercise 2.75 ------------------------------------------------------------------
; Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
	   (* m (cos a)))
          ((eq? op 'imag-part)
	   (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

((make-from-mag-ang 2 12) 'magnitude) ; 2
((make-from-mag-ang 2 12) 'angle) ; 12
((make-from-mag-ang 2 12) 'real-part) ; 1.687707917469843
((make-from-mag-ang 2 12) 'imag-part) ; -1.0731458360008699

; Exercise 2.76 ------------------------------------------------------------------

; As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies -- generic operations with explicit dispatch, data-directed style, and message-passing-style -- describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?

; Generic operations
; All the generic selector must be updated for each new type. Adding a new operation is a matter of creating a new generic selector.

; Data-directed style
; To create a new type you need to make a new install-package procedure. Creating a new operation is easy, you just need to add the operation to the lookup table.

; Message passing
; Creating a new type involves defining a new type constructor, and to add a new operation you need to update
; the existing constructor's definition to handle the operation.

; Data-directed style would be most appropriate where new types are frequently created. Either data-directed or
; message passing styles would be best for an organization where new operations are frequently added.
