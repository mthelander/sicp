; Section 4.2.1 ------------------------------------------------------------------

; Exercise 4.25 ------------------------------------------------------------------

; It will infinitely recurse as it tries to evaluate (factorial (- n 1)),
; unbounded by the base case of n == 1, and eventually resulting in a "maximum
; recursion depth exceeded" error.

; Exercise 4.26 ------------------------------------------------------------------

; Ben is arguing that we could simply implement unless as a derived expression,
; while Alyssa is arguing that we then wouldn't be able to treat unless as a
; procedure.

; We could implement unless as a transformation of an if statement, where the
; arguments to unless are just swapped. However, we can't consider unless as
; a first-class procedure, so if we were to try to pass it to another function
; then it would fail.

; Section 4.2.2 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch4-leval.scm")

(define the-global-environment (setup-environment))

; Exercise 4.27 ------------------------------------------------------------------

;; ;;; L-Eval input:
;; (define count 0)
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (define (id x) (set! count (+ count 1)) x)
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (define w (id (id 10)))
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 1 ; <--- At this point, only the outermost invocation of id has been evaluated,
     ; with the inner one wrapped in a thunk.
;; ;;; L-Eval input:
;; w
;; ;;; L-Eval value:
;; 10 ; <--- Now the value of w is needed, so the delayed argument is evaluated.
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

; Exercise 4.28 ------------------------------------------------------------------

; We have to ensure that the operator is evaluated completely, and if we used
; eval instead of actual-value then it could end up being a delayed object.

; For example:
; ((if (= x 0) + *) 3 5)
; We need to fully evaluate the inner if statement, otherwise we could get a
; delayed object, causing the evaluation to fail.

; Exercise 4.29 ------------------------------------------------------------------

;; ; I would expect the following code to run much faster with memoization:
;; (let ((f (fibonacci 100)))
;;   (display f))
;; ; In this case, f would be evaluated twice.

; WITH memoization:

;; ;;; L-Eval input:
;; (define (square x) (* x x))
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (square (id 10))
;; ;;; L-Eval value:
;; 100
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 1

; WITHOUT memoization:

;; ;;; L-Eval input:
;; (define (square x) (* x x))
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (square (id 10))
;; ;;; L-Eval value:
;; 100
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

; The last value is different because with the memoized version of count has
; already been evaluated, therefore it doesn't recalculate the value.

; Exercise 4.30 ------------------------------------------------------------------

; a)

; Ben is right about the behavior of for-each because newline and display are
; primitive procedures and thus are always forced.

; b)

;; (define (p1 x)
;;   (set! x (cons x '(2)))
;;   x)

;; (define (p2 x)
;;   (define (p e)
;;     e
;;     x)
;;   (p (set! x (cons x '(2)))))

; With original eval-sequence:

; (p1 1) ; (1 2) 
; (p2 1) ; 1

; With Cy's version:

;; (define (eval-sequence exps env)
;;   (cond ((last-exp? exps) (eval (first-exp exps) env))
;;         (else (actual-value (first-exp exps) env)
;;               (eval-sequence (rest-exps exps) env))))

; (p1 1) ; (1 2)
; (p2 1) ; (1 2)

; c)

; Because there are no applications of compound procedures.

; d)

; I like Cy's approach better than Ben's, I think it's better to trade some
; efficiency in order to be sure that everything is evaluated.

; Exercise 4.31 ------------------------------------------------------------------

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments (param-types procedure) env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (make-procedure params body env)
  (define normalized-params
    (map (lambda (x) (if (symbol? x) x (car x))) params))
  (define param-types
    (map (lambda (x) (if (symbol? x) 'non-lazy (cadr x))) params))
  (list 'procedure normalized-params body env param-types))

(define (param-types procedure)
  (car (cddddr procedure)))

(define (delay-it-non-memoized exp env)
  (list 'thunk-non-memoized exp env))
(define (delay-it-memoized exp env)
  (list 'thunk-memoized exp env))

(define (force-it obj)
  (cond ((memoized-thunk? obj) (force-it-memoized obj))
	((non-memoized-thunk? obj) (force-it-non-memoized obj))
	(else obj)))

(define (force-it-non-memoized obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (force-it-memoized obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (memoized-thunk? obj)
  (tagged-list? obj 'thunk-memoized))
(define (non-memoized-thunk? obj)
  (tagged-list? obj 'thunk-non-memoized))
(define (thunk? obj)
  (or (memoized-thunk? obj)
      (non-memoized-thunk? obj)))

(define (maybe-delay-it exp type env)
  (define (delay-proc)
    (case type
      ((non-lazy) actual-value)
      ((lazy) delay-it-non-memoized)
      ((lazy-memo) delay-it-memoized)
      (else (error "Invalid type" type))))
  ((delay-proc) exp env))

(define (list-of-delayed-args exps types env)
  (if (no-operands? exps) '()
      (cons (maybe-delay-it (first-operand exps)
			    (first-operand types) env)
            (list-of-delayed-args (rest-operands exps)
				  (rest-operands types)
                                  env))))

(eval '(begin (define (try a (b lazy))
		(if (= a 0) 1 b))
	      (try 0 (/ 1 0)))
      the-global-environment) ; 1

; Exercise 4.32 ------------------------------------------------------------------

; The difference between the two is that with the lazier lists, the car isn't
; evaluated until it's needed, so the following should evaluate just fine,
; assuming it's constructed correctly:

; (caadr ((/ 1 0) (/ 1 0) 9))

; We could see an improvement with the evaluation of trees, where only the paths
; we care about are evaluated.

; Exercise 4.33 ------------------------------------------------------------------

(define the-global-environment (setup-environment))

; Ugh, I'm not really sure why this works... if I try to modify eval directly
; then I get a max recursion depth error.
(define prev-eval eval)
(define (eval expr env)
  (if (quoted? expr)
      (text-of-quotation expr env)
      (prev-eval expr env)))
; /end

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (symbol? text) text
  	(eval (construct-cons-list text) env))))

(define (construct-cons-list args)
  (if (null? args) (list 'quote '())
      (list 'cons (list 'quote (car args))
	    (construct-cons-list (cdr args)))))

(construct-cons-list '(a b c)) ; (cons 'a (cons 'b (cons 'c '())))

(actual-value '(begin
		 (define (cons x y)
		   (lambda (m) (m x y)))
		 (define (car z)
		   (z (lambda (p q) p)))
		 (define (cdr z)
		   (z (lambda (p q) q))))
	      the-global-environment)
(actual-value '(car (quote (a b c))) the-global-environment) ; a

; Exercise 4.34 ------------------------------------------------------------------

;; (begin
;;   (define (cons x y)
;;     (lambda (m) (m x y)))
;;   (define (car z)
;;     (z (lambda (p q) p)))
;;   (define (cdr z)
;;     (z (lambda (p q) q)))
;;   (define (list-ref items n)
;;     (if (= n 0)
;; 	(car items)
;; 	(list-ref (cdr items) (- n 1))))
;;   (define (map proc items)
;;     (if (null? items)
;; 	'()
;; 	(cons (proc (car items))
;; 	      (map proc (cdr items)))))
;;   (define (scale-list items factor)
;;     (map (lambda (x) (* x factor))
;; 	 items))
;;   (define (add-lists list1 list2)
;;     (cond ((null? list1) list2)
;; 	  ((null? list2) list1)
;; 	  (else (cons (+ (car list1) (car list2))
;; 		      (add-lists (cdr list1) (cdr list2))))))
;;   (define ones (cons 1 ones))
;;   (define integers (cons 1 (add-lists ones integers))))

(define the-global-environment (setup-environment))

(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list 'lazy-pair
			(actual-value (car object) the-global-environment)
			'cdr)))
	 ;; (display (list 'compound-procedure
	 ;; 		(procedure-parameters object)
	 ;; 		(procedure-body object)
	 ;; 		'<procedure-env>)))
	((pair? object)
	 (display (car object)))
	(else (display object))))

; This exercise is lame. Skip it for now.
