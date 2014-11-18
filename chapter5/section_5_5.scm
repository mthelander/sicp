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
;;  ((assign val (op make-compiled-procedure) (label entry2) (reg env))
;;     (goto (label after-lambda1))
;;   entry2
;;     (assign env (op compiled-procedure-env) (reg proc))
;;     (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;     (save continue)
;;     (save env)
;;     (assign proc (op lookup-variable-value) (const =) (reg env))
;;     (assign val (const 1))
;;     (assign argl (op list) (reg val))
;;     (assign val (op lookup-variable-value) (const n) (reg env))
;;     (assign argl (op cons) (reg val) (reg argl))
;;     (test (op primitive-procedure?) (reg proc))
;;     (branch (label primitive-branch17))
;;   compiled-branch16
;;     (assign continue (label after-call15))
;;     (assign val (op compiled-procedure-entry) (reg proc))
;;     (goto (reg val))
;;   primitive-branch17
;;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   after-call15
;;     (restore env)
;;     (restore continue)
;;     (test (op false?) (reg val))
;;     (branch (label false-branch4))
;;   true-branch5
;;     (assign val (const 1))
;;     (goto (reg continue))
;;   false-branch4
;;     (assign proc (op lookup-variable-value) (const *) (reg env))
;;     (save continue)
;;     (save proc)
;;     (save env)
;;     (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
;;     (save proc)
;;     (assign proc (op lookup-variable-value) (const -) (reg env))
;;     (assign val (const 1))
;;     (assign argl (op list) (reg val))
;;     (assign val (op lookup-variable-value) (const n) (reg env))
;;     (assign argl (op cons) (reg val) (reg argl))
;;     (test (op primitive-procedure?) (reg proc))
;;     (branch (label primitive-branch8))
;;   compiled-branch7
;;     (assign continue (label after-call6))
;;     (assign val (op compiled-procedure-entry) (reg proc))
;;     (goto (reg val))
;;   primitive-branch8
;;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   after-call6
;;     (assign argl (op list) (reg val))
;;     (restore proc)
;;     (test (op primitive-procedure?) (reg proc))
;;     (branch (label primitive-branch11))
;;   compiled-branch10
;;     (assign continue (label after-call9))
;;     (assign val (op compiled-procedure-entry) (reg proc))
;;     (goto (reg val))
;;   primitive-branch11
;;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   after-call9
;;     (assign argl (op list) (reg val))
;;     (restore env)
;;     (assign val (op lookup-variable-value) (const n) (reg env))
;;     (assign argl (op cons) (reg val) (reg argl))
;;     (restore proc)
;;     (restore continue)
;;     (test (op primitive-procedure?) (reg proc))
;;     (branch (label primitive-branch14))
;;   compiled-branch13
;;     (assign val (op compiled-procedure-entry) (reg proc))
;;     (goto (reg val))
;;   primitive-branch14
;;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;     (goto (reg continue))
;;   after-call12
;;   after-if3
;;   after-lambda1
;;     (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
;;     (assign val (const ok)))
 

(pretty-print-code (statements (compile
				'(define (factorial n)
				   (if (= n 1)
				       1
				       (* (factorial (- n 1)) n)))
				'val
				'next)))

;;   (assign val (op make-compiled-procedure) (label entry19) (reg env))
;;   (goto (label after-lambda18))
;; entry19
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;   (save continue)
;;   (save env)
;;   (assign proc (op lookup-variable-value) (const =) (reg env))
;;   (assign val (const 1))
;;   (assign argl (op list) (reg val))
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch34))
;; compiled-branch33
;;   (assign continue (label after-call32))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch34
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call32
;;   (restore env)
;;   (restore continue)
;;   (test (op false?) (reg val))
;;   (branch (label false-branch21))
;; true-branch22
;;   (assign val (const 1))
;;   (goto (reg continue))
;; false-branch21
;;   (assign proc (op lookup-variable-value) (const *) (reg env))
;;   (save continue)
;;   (save proc)
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   (assign argl (op list) (reg val))
;;   (save argl)
;;   (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;   (save proc)
;;   (assign proc (op lookup-variable-value) (const -) (reg env))
;;   (assign val (const 1))
;;   (assign argl (op list) (reg val))
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch25))
;; compiled-branch24
;;   (assign continue (label after-call23))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch25
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call23
;;   (assign argl (op list) (reg val))
;;   (restore proc)
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch28))
;; compiled-branch27
;;   (assign continue (label after-call26))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch28
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call26
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore proc)
;;   (restore continue)
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch31))
;; compiled-branch30
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch31
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))
;; after-call29
;; after-if20
;; after-lambda18
;;   (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;   (assign val (const ok))
