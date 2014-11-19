; Exercise 5.31 ------------------------------------------------------------------

;; (f 'x 'y)

; They are all superfluous here, since we could directly lookup the values.

;; ((f) 'x 'y)

; The only saves and restores that are necessary are around evaluating the
; operator. We could lookup the values for the operands directly.

;; (f (g 'x) y)

; Only the saves/restores around the operator could be omitted here, the
; operands need to be eval'ed. 

;; (f (g 'x) 'y)

; We still need them for eval'ing the first operand, but not the rest.

; Exercise 5.32 ------------------------------------------------------------------

; a.

(load "~/work/sicp/from_book/ch5-regsim.scm")
(load "~/work/sicp/from_book/ch5-eceval-support.scm")
(load "~/work/sicp/from_book/ch5-eceval.scm")
(load "~/work/sicp/chapter5/eceval-components.scm")

(define ev-application
  '(
ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp))
  (branch (label ev-application-symbol-operator))
  (save env)
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-application-symbol-operator
  (assign continue (label ev-appl-did-operator-sans-restores))
  (goto (label ev-variable))
ev-appl-did-operator
  (restore unev)
  (restore env)
ev-appl-did-operator-sans-restores
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))))

(define the-global-environment (setup-environment))

(define eceval-extended-operations
  (append eceval-operations
	  `((exit? ,exit?)
	    (symbol? ,symbol?)
	    (eq? ,eq?))))

(define (make-eceval)
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-extended-operations
   (eceval-body)))

(define eceval (make-eceval))

(define (run-it)
  (start eceval)
  (get-register-contents eceval 'val))

(with-input-from-string "(+ 5 8)" run-it)
;; ;;; EC-Eval input:

;; (total-pushes = 6 maximum-depth = 5)
;; ;;; EC-Eval value:
;; 13

;; ;;; EC-Eval input:
;; ;Value: 13

; We can see that this is 2 fewer total-pushes than before. QED.

; b.

; I think Alyssa's idea is a terrible one. The advantage with compilation is that
; it can be performed once, and the generated code is tailored to the exact input.
; Alyssa's idea would defer that to runtime and bloat the final code that is 
; interpreted.

; Section 5.5.5 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch5-compiler.scm")

; Exercise 5.33 ------------------------------------------------------------------

(pretty-print-code (statements (compile
				'(define (factorial n)
				   (if (= n 1)
				       1
				       (* (factorial (- n 1)) n)))
				'val
				'next)))

(define (pretty-print-code code)
  (for-each (lambda (el)
  	      (if (pair? el) (display "  "))
  	      (display el)
  	      (newline))
  	    code))

(define factorial-alt-code (compile '(define (factorial-alt n)
				       (if (= n 1) 1
					   (* n (factorial-alt (- n 1)))))
				    'val
				    'next))

(pretty-print-code (statements factorial-alt-code))

;; 33,36c33,34
;; < (assign val (op lookup-variable-value) (const n) (reg env))
;; < (assign argl (op list) (reg val))
;; < (save argl)
;; < (assign proc (op lookup-variable-value) (const factorial) (reg env))
;; ---
;; > (save env)
;; > (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
;; 63c61,63
;; < (restore argl)
;; ---
;; > (assign argl (op list) (reg val))
;; > (restore env)
;; > (assign val (op lookup-variable-value) (const n) (reg env))
;; 78c78
;; < (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; ---
;; > (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))


; The only difference is that the operands are evaluated in the reverse order.
; The first version is actually slightly more efficient because since the
; first operand evaluated requires env, it doesn't need to push it onto
; the stack.

; Exercise 5.34 ------------------------------------------------------------------

(define factorial-iterative-code (compile '(define (factorial n)
					     (define (iter product counter)
					       (if (> counter n)
						   product
						   (iter (* counter product)
							 (+ counter 1))))
					     (iter 1 1))
					  'val
					  'next))

(pretty-print-code (statements factorial-iterative-code))

; This is the part of the code that allows it to be tail recursive:

;; compiled-branch52
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val)

; poc contains a reference to the iter function, and the evaluator simply
; jumps to the next iteration, without having to add to the continue stack.

; Contrast that with the code for the recursive version:

;; compiled-branch10
;;   (assign continue (label after-call9))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; ...
;; after-call9
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore proc)
;;   (restore continue)
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch14))

; Every recursive call must add to the stack, and then remove the info
; after the call has returned. Therefore the iterative procedure runs in
; constant stack space, and the recursive one does not. QED.

; Exercise 5.35 ------------------------------------------------------------------

(define f-code '(define (f)
		  (+ (g (+ x 2)) x)))

(pretty-print-code (statements (compile f-code 'val 'next)))
