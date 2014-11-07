; Section 5.4.1 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch5-regsim.scm")
(load "~/work/sicp/from_book/ch5-eceval-support.scm")
(load "~/work/sicp/from_book/ch5-eceval.scm")

; Exercise 5.23 ------------------------------------------------------------------

(load "~/work/sicp/chapter5/eceval-components.scm")

(define the-global-environment (setup-environment))

(define (run-it)
  (start extended-eceval)
  (get-register-contents extended-eceval 'val))

(define (exit? exp)
  (eof-object? exp))

; FROM EXERCISE 4.6

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-pairs exp) (cadr exp))
(define (let-vars exp) (map car (let-pairs exp)))
(define (let-values exp) (map cadr (let-pairs exp)))

(define (let->combination exp)
  (cons (cons 'lambda
	      (cons (let-vars exp)
		    (let-body exp)))
	(let-values exp)))

; END 4.6

(define ev-cond
  '(ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))))

(define ev-let
  '(ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))))

(define eceval-extended-operations
  (append eceval-operations
	  `((exit? ,exit?)
	    (cond? ,cond?)
	    (cond->if ,cond->if)
	    (let? ,let?)
	    (let->combination ,let->combination))))

(define (eceval-body)
  (append read-eval-print-loop
	  print-result
	  unknown-expression-type
	  unknown-procedure-type
	  signal-error
	  eval-dispatch
	  ev-self-eval
	  ev-application
	  primitive-apply
	  coumpound-apply
	  ev-begin
	  ev-let
	  ev-cond
	  ev-sequence
	  ev-if
	  ev-assignment
	  ev-definition))

(define (make-extended-eceval)
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-extended-operations
   (eceval-body)))

(define extended-eceval (make-extended-eceval))

(with-input-from-string "(cond ((= 3 2) 9) ((= 5 5) 8) (else 17))" run-it) ; 8
(with-input-from-string "(let ((foo (if true '(stuff) 5))) foo)" run-it) ; (stuff)

; Exercise 5.24 ------------------------------------------------------------------

(define eceval-extended-operations
  (append eceval-operations
	  `(
	    (exit? ,exit?)
	    (cond? ,cond?)
	    (cond->if ,cond->if)
	    (let? ,let?)
	    (let->combination ,let->combination)
	    (cond-clauses ,cond-clauses)
	    (cond-predicate ,cond-predicate)
	    (cond-else-clause? ,cond-else-clause?)
	    (cond-predicate ,cond-predicate)
	    (cond-actions ,cond-actions)
	    (car ,car)
	    (cdr ,cdr)
	    (null? ,null?)
	    (display ,display)
	   )))

(define ev-cond
  '(ev-cond 
     (assign unev (op cond-clauses) (reg exp)) ; Assign the clauses to unev
   
   ev-cond-expand-clauses
     (test (op null?) (reg unev))
     (branch (label ev-cond-last-exp))
     (save unev)
     (assign unev (op car) (reg unev)) ; unev is now the first clause
     (test (op cond-else-clause?) (reg unev)) ; else clauses are always true
     (branch (label ev-else-clause))
     (assign exp (op cond-predicate) (reg unev)) ; exp is now the first predicate
     (save continue)
     (assign continue (label ev-cond-expand-clause-eval))
     (goto (label eval-dispatch))

   ev-cond-expand-clause-eval
     (restore continue)
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-clause-is-true))
     (goto (label ev-cond-clause-is-false))

   ev-else-clause
     (restore unev)

   ev-cond-clause-is-true
     (assign unev (op car) (reg unev))
     (assign unev (op cond-actions) (reg unev))
     (save continue)
     (goto (label ev-sequence))

   ev-cond-clause-is-false
     (assign unev (op cdr) (reg unev))
     (goto (label ev-cond-expand-clauses))

   ev-cond-last-exp
     (assign val (const #!unspecific))
     (goto (reg continue))))

(define extended-eceval (make-extended-eceval))

(with-input-from-string "(cond ((= 3 2) 9) ((= 5 5) 8) (else 17))" run-it) ; 8
(with-input-from-string "(cond ((= 3 2) 9) ((= 5 6) 8) (else 17))" run-it) ; 17
(with-input-from-string "(cond ((= 3 2) 9) ((= 5 6) 8))" run-it) ; Unpecified return value

; Exercise 5.25 ------------------------------------------------------------------

(define lazy-ev-application
  '(
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label actual-value)) ; NEW

ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch)) ; CHANGE
  (save proc)
  ; NEW
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-ev-appl-operand-loop))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-ev-appl-operand-loop))
  (goto (label unknown-procedure-type))

primitive-ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label primitive-ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label primitive-ev-appl-accumulate-arg))
  (goto (label actual-value))
primitive-ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label primitive-ev-appl-operand-loop))
primitive-ev-appl-last-arg
  (assign continue (label primitive-ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
primitive-ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label primitive-apply))

compound-ev-appl-operand-loop
  (test (op null?) (reg unev))
  (branch (label compound-no-more-operands))
  (assign exp (op first-operand) (reg unev))
  (assign val (op delay-it) (reg exp) (reg env))
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label compound-ev-appl-operand-loop))
compound-no-more-operands
  (restore proc)
  (goto (label compound-apply))
; /NEW

apply-dispatch
  ; Most of this is redundant, but oh well.
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

actual-value
  (save continue)
  (assign continue (label force-it))
  (goto (label eval-dispatch))

force-it
  (restore continue)
  (test (op thunk?) (reg val))
  (branch (label force-a-thunk))
  (goto (reg continue)) ; val already contains the value

force-a-thunk
  (assign exp (op thunk-exp) (reg val))
  (assign env (op thunk-env) (reg val))
  (goto (label actual-value))))

; From 4.2
(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
; End 4.2

(define lazy-eceval-operations
  (append eceval-extended-operations
	  `(
	    (exit? ,exit?)
	    (cond? ,cond?)
	    (cond->if ,cond->if)
	    (let? ,let?)
	    (let->combination ,let->combination)
	    (cond-clauses ,cond-clauses)
	    (cond-predicate ,cond-predicate)
	    (cond-else-clause? ,cond-else-clause?)
	    (cond-predicate ,cond-predicate)
	    (cond-actions ,cond-actions)
	    (car ,car)
	    (cdr ,cdr)
	    (null? ,null?)
	    (display ,display)
	    (thunk? ,thunk?) ; NEW
	    (thunk-exp ,thunk-exp) ; NEW
	    (thunk-env ,thunk-env) ; NEW
	    (delay-it ,delay-it) ; NEW
	   )))

(define (lazy-eceval-body)
  (append read-eval-print-loop
	  print-result
	  unknown-expression-type
	  unknown-procedure-type
	  signal-error
	  eval-dispatch
	  ev-self-eval
	  lazy-ev-application ; CHANGED
	  primitive-apply
	  coumpound-apply
	  ev-begin
	  ev-let
	  ev-cond
	  ev-sequence
	  ev-if
	  ev-assignment
	  ev-definition))

(define (make-lazy-eceval)
  (make-machine
   '(exp env val proc argl continue unev)
   lazy-eceval-operations
   (lazy-eceval-body)))

(define lazy-eceval (make-lazy-eceval))

(define (run-it-lazy)
  (start lazy-eceval)
  (get-register-contents lazy-eceval 'val))

(with-input-from-string "(define (try a b) (if (= a 0) 1 b)) (try 0 (/ 1 0))"
  run-it-lazy) ; 1

; Exercise 5.26 ------------------------------------------------------------------

(define fact-source
  "(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
	   (iter (* counter product)
	         (+ counter 1))))
     (iter 1 1))")

(define (call-fact-source n)
  (string-append "(factorial " (number->string n) ")"))

(with-input-from-string fact-source run-it)
(with-input-from-string (call-fact-source 3) run-it)
;; (total-pushes = 134 maximum-depth = 10)
(with-input-from-string (call-fact-source 4) run-it)
;; (total-pushes = 169 maximum-depth = 10)
(with-input-from-string (call-fact-source 5) run-it)
;; (total-pushes = 204 maximum-depth = 10)
(with-input-from-string (call-fact-source 6) run-it)
;; (total-pushes = 239 maximum-depth = 10)

; a.

;; The maximum depth is 10

; b.

;; = (35*(n+1))-6
;; = 35n+35-6
;; = 35n+29

;; Proof:

(map (lambda (n) (+ (* 35 n) 29)) '(3 4 5 6)) ; (134 169 204 239)

; Exercise 5.27 ------------------------------------------------------------------

(define fact-source
  "(define (factorial n)
     (if (= n 1) 1
	 (* (factorial (- n 1)) n)))")

(with-input-from-string fact-source run-it)
(with-input-from-string (call-fact-source 3) run-it)
;; (total-pushes = 80 maximum-depth = 18)
(with-input-from-string (call-fact-source 4) run-it)
;(total-pushes = 112 maximum-depth = 23)
(with-input-from-string (call-fact-source 5) run-it)
;; (total-pushes = 144 maximum-depth = 28)
(with-input-from-string (call-fact-source 6) run-it)
;; (total-pushes = 176 maximum-depth = 33)

; Proof:

(map (lambda (n) (+ (* 5 n) 3)) '(3 4 5 6)) ; (18 23 28 33)
(map (lambda (n) (* 16 (- (* 2 n) 1))) '(3 4 5 6)) ; (80 112 144 176)

;; +-----------+---------------+------------------+
;; |           | Maximum depth | Number of pushes |
;; +-----------+---------------+------------------+
;; | Recursive | 5n+3          | 16(2n-1)         |
;; | Factorial |               |                  |
;; +-----------+---------------+------------------+
;; | Iterative | 35n+29        | 10               |
;; | Factorial |               |                  |
;; +-----------+---------------+------------------+

; Exercise 5.28 ------------------------------------------------------------------

(define (make-non-tr-eceval)
  (define (no-more-exps? seq) (null? seq))
  (define ev-sequence
    '(ev-sequence
       (test (op no-more-exps?) (reg unev))
       (branch (label ev-sequence-end))
       (assign exp (op first-exp) (reg unev))
       (save unev)
       (save env)
       (assign continue (label ev-sequence-continue))
       (goto (label eval-dispatch))
     ev-sequence-continue
       (restore env)
       (restore unev)
       (assign unev (op rest-exps) (reg unev))
       (goto (label ev-sequence))
     ev-sequence-end
       (restore continue)
       (goto (reg continue))))
  (make-machine
   '(exp env val proc argl continue unev)
   (cons `(no-more-exps? ,no-more-exps?)
	 eceval-extended-operations)
   (append read-eval-print-loop
   	   print-result
   	   unknown-expression-type
   	   unknown-procedure-type
   	   signal-error
   	   eval-dispatch
   	   ev-self-eval
   	   ev-application
   	   primitive-apply
   	   coumpound-apply
   	   ev-begin
   	   ev-let
   	   ev-cond
   	   ev-sequence
   	   ev-if
   	   ev-assignment
   	   ev-definition)))

(define non-tail-recursive-eceval (make-non-tr-eceval))

(define (run-it-non-tr)
  (start non-tail-recursive-eceval)
  (get-register-contents non-tail-recursive-eceval 'val))

(define fact-source
  "(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
	   (iter (* counter product)
	         (+ counter 1))))
     (iter 1 1))")

(with-input-from-string fact-source run-it-non-tr)
(with-input-from-string (call-fact-source 3) run-it-non-tr)
;; (total-pushes = 144 maximum-depth = 23)
(with-input-from-string (call-fact-source 4) run-it-non-tr)
;; (total-pushes = 181 maximum-depth = 26)
(with-input-from-string (call-fact-source 5) run-it-non-tr)
;; (total-pushes = 218 maximum-depth = 29)
(with-input-from-string (call-fact-source 6) run-it-non-tr)
;; (total-pushes = 255 maximum-depth = 32)

(define fact-source
  "(define (factorial n)
     (if (= n 1) 1
	 (* (factorial (- n 1)) n)))")

(with-input-from-string fact-source run-it)
(with-input-from-string (call-fact-source 3) run-it-non-tr)
;; (total-pushes = 86 maximum-depth = 27)
(with-input-from-string (call-fact-source 4) run-it-non-tr)
;; (total-pushes = 120 maximum-depth = 35)
(with-input-from-string (call-fact-source 5) run-it-non-tr)
;; (total-pushes = 154 maximum-depth = 43)
(with-input-from-string (call-fact-source 6) run-it-non-tr)
;; (total-pushes = 188 maximum-depth = 51)

;; See! They are the space is a function of n in both cases.

; Exercise 5.29 ------------------------------------------------------------------

(define fib-source
  "(define (fib n)
    (if (< n 2)	n
	(+ (fib (- n 1)) (fib (- n 2)))))")

(define (call-fib-source n)
  (string-append "(fib " (number->string n) ")"))

; a.

(with-input-from-string fib-source run-it)
(with-input-from-string (call-fib-source 2) run-it)
;; (total-pushes = 72 maximum-depth = 13)
(with-input-from-string (call-fib-source 3) run-it)
;; (total-pushes = 128 maximum-depth = 18)
(with-input-from-string (call-fib-source 4) run-it)
;; (total-pushes = 240 maximum-depth = 23)
(with-input-from-string (call-fib-source 5) run-it)
;; (total-pushes = 408 maximum-depth = 28)

; 5n+3

; Proof:

(map (lambda (n) (+ (* 5 n) 3)) '(2 3 4 5)) ; (13 18 23 28)

; b.

(with-input-from-string fib-source run-it)
(with-input-from-string (call-fib-source 2) run-it)
;; (total-pushes = 72 maximum-depth = 13)
(with-input-from-string (call-fib-source 3) run-it)
;; (total-pushes = 128 maximum-depth = 18)
(with-input-from-string (call-fib-source 4) run-it)
;; (total-pushes = 240 maximum-depth = 23)
(with-input-from-string (call-fib-source 5) run-it)
;; (total-pushes = 408 maximum-depth = 28)
(with-input-from-string (call-fib-source 6) run-it)
;; (total-pushes = 688 maximum-depth = 33)

; S(n)=S(n-1)+S(n-2)+k
; S(4)=128+72+k
; 240=200+k
; k=240-200
; k=40

; S(3)=72+S(1)+40
; S(3)=112+S(1)
; 128=112+S(1)
; S(1)=128-112
; S(1)=16

; S(2)=16+S(0)+40
; S(2)=56+S(0)
; 72=56+S(0)
; S(0)=72-56
; S(0)=16

; S(1)=16
; S(0)=16

; It's exponential because of the recursive definition of S(n), which
; forms a tree. A tree has 2^n leaf nodes.

; S(n)=a*fib(n+1)+b
; S(4)=a*fib(5)+b
; 240=a*5+b

; 72=a*2+b
; b=72-2a

; 240=5a+b
; 240=5a+72-2a
; 240=3a+72
; 3a=240-72
; 3a=168
; a=168/3
; a=56

; 240=5(56)+b
; 240=280+b
; b=240-280
; b=-40

; a is 56, b is -40.

; Proof:

(define (S n)
  (if (< n 2) 16
      (+ (S (- n 1))
	 (S (- n 2))
	 40)))

(map S '(1 2 3 4 5 6)) ; (16 72 128 240 408 688)

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(define (fib-S n)
  (- (* 56 (fib (+ n 1)))
     40))

(map fib-S '(1 2 3 4 5 6)) ; (16 72 128 240 408 688)
