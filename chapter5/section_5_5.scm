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

(pretty-print-code (statements (compile
				'(define (factorial n)
				   (if (= n 1)
				       1
				       (* (factorial (- n 1)) n)))
				'val
				'next)))

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

; proc contains a reference to the iter function, and the evaluator simply
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

; Exercise 5.36 ------------------------------------------------------------------

; It is right-to-left order, and that is determined by construct-arglist. The
; first line in the procedure reverses the list of operands and then emits the
; compiled code from left-to-right.

; This version of construct-arglist would generate code that evaluates in
; left-to-right order:

;; (define (construct-arglist operand-codes)
;;   (if (null? operand-codes)
;;       (make-instruction-sequence '() '(argl)
;; 				 '((assign argl (const ()))))
;;       (let ((code-to-get-last-arg
;; 	     (append-instruction-sequences
;; 	      (car operand-codes)
;; 	      (make-instruction-sequence '(val) '(argl)
;; 					 '((assign argl (op list) (reg val)))))))
;; 	(if (null? (cdr operand-codes))
;; 	    code-to-get-last-arg
;; 	    (preserving '(env)
;; 			code-to-get-last-arg
;; 			(code-to-get-rest-args
;; 			 (cdr operand-codes)))))))

;; (pretty-print-code (statements (compile '(+ 1 2 3) 'val 'next)))

; Exercise 5.37 ------------------------------------------------------------------

; With optimized preserving:

;; (pretty-print-code (statements (compile '(+ 1 2 3) 'val 'next)))

;;   (assign proc (op lookup-variable-value) (const +) (reg env))
;;   (assign val (const 3))
;;   (assign argl (op list) (reg val))
;;   (assign val (const 2))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (assign val (const 1))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch70))
;; compiled-branch69
;;   (assign continue (label after-call68))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch70
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call68

; Without optimized preserving:

;; (define (preserving regs seq1 seq2)
;;   (if (null? regs)
;;       (append-instruction-sequences seq1 seq2)
;;       (let ((first-reg (car regs)))
;; 	(preserving (cdr regs)
;; 		    (make-instruction-sequence
;; 		     (list-union (list first-reg)
;; 				 (registers-needed seq1))
;; 		     (list-difference (registers-modified seq1)
;; 				      (list first-reg))
;; 		     (append `((save ,first-reg))
;; 			     (statements seq1)
;; 			     `((restore ,first-reg))))
;; 		    seq2))))

;; (pretty-print-code (statements (compile '(+ 1 2 3) 'val 'next)))

;;   (save continue)
;;   (save env)
;;   (save continue)
;;   (assign proc (op lookup-variable-value) (const +) (reg env))
;;   (restore continue)
;;   (restore env)
;;   (restore continue)
;;   (save continue)
;;   (save proc)
;;   (save env)
;;   (save continue)
;;   (assign val (const 3))
;;   (restore continue)
;;   (assign argl (op list) (reg val))
;;   (restore env)
;;   (save env)
;;   (save argl)
;;   (save continue)
;;   (assign val (const 2))
;;   (restore continue)
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore env)
;;   (save argl)
;;   (save continue)
;;   (assign val (const 1))
;;   (restore continue)
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore proc)
;;   (restore continue)
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch70))
;; compiled-branch69
;;   (assign continue (label after-call68))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;; primitive-branch70
;;   (save continue)
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (restore continue)
;; after-call68
;; ;Unspecified return value

; In this example, all the saves and restores are unnecessary. So 13 saves and
; restores.

; Exercise 5.38 ------------------------------------------------------------------

; a.

(define (spread-arguments operand-list)
  (map (lambda (op reg) (compile op reg 'next))
       operand-list
       '(arg1 arg2)))

; b.

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
	((open-code-primitive? exp) ; NEW
	 (dispatch-to-open-code-primitive exp target linkage)) ; NEW
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (open-code-primitive? exp)
  (and (pair? exp)
       (memq (operator exp) '(= * - +))))

(define (dispatch-to-open-code-primitive exp target linkage)
  (end-with-linkage
   linkage
   (compile-open-code-primitive (operator exp) (operands exp) target)))

(define (compile-open-code-primitive op operands target)
  (let ((args (spread-arguments operands)))
    (append-instruction-sequences
     (car args)
     (preserving '(arg1)
		 (cadr args)
		 (make-instruction-sequence
		  '(arg1 arg2)
		  (list target)
		  `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))

; c.

(pretty-print-code (statements (compile
				'(define (factorial n)
				   (if (= n 1) 1
				       (* (factorial (- n 1)) n)))
				'val
				'next)))

;; Yep, the output is significantly shorter. It doesn't have the
; compiled procedure branches of the original.

(pretty-print-code (statements (compile '(+ 8 (+ 3 5)) 'val 'next)))
;; (assign arg1 (const 8))
;; (save arg1)
;; (assign arg1 (const 3))
;; (assign arg2 (const 5))
;; (assign arg2 (op +) (reg arg1) (reg arg2))
;; (restore arg1)
;; (assign val (op +) (reg arg1) (reg arg2))

; d.

(define (handles-n-args? op)
  (memq op '(* +)))

(define (dispatch-to-open-code-primitive exp target linkage)
  (let ((op (operator exp))
	(args (operands exp)))
    (end-with-linkage
     linkage
     (compile-open-code-primitive op (expand-args op args) target))))

(define (two-remaining? args)
  (= (length args) 2))

(define (expand-args op args)
  (if (not (handles-n-args? op)) args
      (if (two-remaining? args) args
	  (cons (car args)
		(list (cons op (expand-args op (cdr args))))))))

(expand-args '+ '(3 4 5)) ; (3 (+ 4 5))
(expand-args '+ '(2 3 5 8 13 21)) ; (2 (+ 3 (+ 5 (+ 8 (+ 13 21)))))

(pretty-print-code (statements (compile '(+ 8 9 10 11 12 13) 'val 'next)))
;; (assign arg1 (const 8))
;; (save arg1)
;; (assign arg1 (const 9))
;; (save arg1)
;; (assign arg1 (const 10))
;; (save arg1)
;; (assign arg1 (const 11))
;; (save arg1)
;; (assign arg1 (const 12))
;; (assign arg2 (const 13))
;; (assign arg2 (op +) (reg arg1) (reg arg2))
;; (restore arg1)
;; (assign arg2 (op +) (reg arg1) (reg arg2))
;; (restore arg1)
;; (assign arg2 (op +) (reg arg1) (reg arg2))
;; (restore arg1)
;; (assign arg2 (op +) (reg arg1) (reg arg2))
;; (restore arg1)
;; (assign val (op +) (reg arg1) (reg arg2))

; Exercise 5.39 ------------------------------------------------------------------

(define (make-address x y) (cons x y))
(define (frames-offset a) (car a))
(define (vars-offset a) (cdr a))

(define (scan-frames address env proc)
  (let ((x (frames-offset address))
	(y (vars-offset address)))
    (if (= 0 x)
	(proc y (first-frame env))
	(scan-frames (make-address (- x 1) y)
		     (enclosing-environment env)
		     proc))))

(define (lexical-address-lookup address env)
  (scan-frames address env get-value-or-die))

(define (get-value-or-die index frame)
  (let ((value (list-ref (frame-values frame) index)))
    (if (eq? value '*unassigned*)
	(error "Unassigned variable!")
	value)))

(define (lexical-address-set! address env value)
  (define (list-from n l)
    (if (= n 0) l
	(list-from (- n 1) (cdr l))))
  (scan-frames address env
	       (lambda (index frame)
		 (set-car! (list-from index (frame-values frame))
			   value))))

(define test-env
  (extend-environment '(a b c) '(1 2 3)
      (extend-environment '(e f g) '(4 5 6)
	  (extend-environment '(h i j k l) '(7 8 9 10 11)
	      (extend-environment '(m n o p) '(12 *unassigned* 13 14)
		  the-empty-environment)))))

(lexical-address-lookup (make-address 2 2) test-env) ; 9
;; (lexical-address-lookup (make-address 3 1) test-env) ; Unassigned variable!

(lexical-address-set! (make-address 2 2) test-env 58)
(lexical-address-lookup (make-address 2 2) test-env) ; 58

; Exercise 5.40 ------------------------------------------------------------------

(load "~/work/sicp/chapter5/env-compiler.scm")

(define (compile-lambda exp target linkage env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env))))
	 env)
        (compile-lambda-body exp proc-entry env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return
		       (cons formals env))))) ; <-- NEW

(pretty-print-code (statements (compile
				'(define (factorial n)
				   (if (= n 1)
				       1
				       (* (factorial (- n 1)) n)))
				'val
				'next
				the-empty-environment)))

; Exercise 5.41 ------------------------------------------------------------------

(define (find-variable var env)
  (define (move-right address)
    (make-address (frames-offset address)
		  (+ 1 (vars-offset address))))
  (define (move-up address)
    (make-address (+ 1 (frames-offset address))
		  (vars-offset address)))
  (define (scan-env address env)
    (define (scan-vars vars vars-address)
      (cond ((null? vars) (scan-env (move-up address) (cdr env)))
	    ((eq? (car vars) var) vars-address)
	    (else (scan-vars (cdr vars) (move-right vars-address)))))
    (if (null? env)
	'not-found
	(scan-vars (car env) address)))
  (scan-env (make-address 0 0) env))

(find-variable 'c '((y z) (a b c d e) (x y))) ; (1 . 2)
(find-variable 'x '((y z) (a b c d e) (x y))) ; (2 . 0)
(find-variable 'w '((y z) (a b c d e) (x y))) ; not-found

; Exercise 5.42 ------------------------------------------------------------------

(define (compile-variable exp target linkage env)
  (define (instructions-for-global-env)
    (make-instruction-sequence '(env) (list target env)
       `((save env) ; NOTE: This save/restore pair might be superfluous...
	 (assign env (op get-global-environment))
	 (assign ,target (op lookup-variable-value) (const ,exp) (reg env))
	 (restore env))))
  (define (instructions-for-lexical-address)
    (make-instruction-sequence '(env) (list target env)
       `((assign ,target (op lexical-address-lookup) (const ,exp) (reg env)))))

  (let ((address (find-variable exp env)))
    (end-with-linkage linkage
      (if (eq? address 'not-found)
	  (instructions-for-global-env)
	  (instructions-for-lexical-address))
      env)))

(define (compile-assignment exp target linkage env)
  (let* ((var (assignment-variable exp))
	 (get-value-code (compile (assignment-value exp) 'val 'next env))
	 (address (find-variable var env)))
    (define (instructions-for-global-env)
      (make-instruction-sequence '(env val) (list target env)
	 `((save env) ; NOTE: This save/restore pair might be superfluous...
	   (assign env (op get-global-environment))
	   (perform (op set-variable-value!)
		    (const ,var)
		    (reg val)
		    (reg env))
	   (restore env)
	   (assign ,target (const ok)))))
    (define (instructions-for-lexical-address)
      (make-instruction-sequence '(env val) (list target)
	 `((perform (op lexical-address-set!)
		    (const ,var)
		    (reg val)
		    (reg env))
	   (assign ,target (const ok)))))

    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (if (eq? address 'not-found)
	  (instructions-for-global-env)
	  (instructions-for-lexical-address)))
     env)))

(define (compile-and-print exp)
  (pretty-print-code
   (statements
    (compile exp 'val 'next the-global-environment))))

(compile-and-print 'x)
;; (save env)
;; (assign env (op get-global-environment))
;; (assign val (op lookup-variable-value) (const x) (reg env))
;; (restore env)

(compile-and-print '(set! x 144))
;; (assign val (const 144))
;; (save env)
;; (assign env (op get-global-environment))
;; (perform (op set-variable-value!) (const x) (reg val) (reg env))
;; (restore env)
;; (assign val (const ok))

(compile-and-print '(let ((x 3) (y 4))
		      (lambda (a b c d e)
			(let ((y (* a b x))
			      (z (+ c d x)))
			  (* x y z)))))

(compile-and-print '((lambda (x y)
		       (lambda (a b c d e)
			 ((lambda (y z) (* x y z))
			  (* a b x)
			  (+ c d x))))
		     3
		     4))

; Exercise 5.43 ------------------------------------------------------------------

(define (scan-out-defines body)
  (let ((defines (filter definition? body)))
    (if (null? defines)
	body
	(let ((vars-and-values (map (lambda (d)
				      (list (definition-variable d)
					    ''*unassigned*))
				    defines)))
	  `(let ,vars-and-values
	     (begin ,@(map (lambda (d)
	  		    (list 'set! (definition-variable d)
	  			  (definition-value d)))
	  		  defines))
	     ,@(remove definition? body))))))

(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return env)))) ; NEW

(scan-out-defines '((define u (+ 1 2 3))
		    (define v (* 1 2 3))
		    (+ u v)
		    (* u v)))
;; (let ((u (quote *unassigned*))
;;       (v (quote *unassigned*)))
;;   (begin (set! u (+ 1 2 3))
;; 	 (set! v (* 1 2 3)))
;;   (+ u v)
;;   (* u v))

(scan-out-defines '((define u (+ 1 2 3))))
;; (let ((u (quote *unassigned*)))
;;   (begin (set! u (+ 1 2 3))))

(compile-and-print '(define (foobar a b)
		      (define u (+ 5 8))
		      (define v (* 13 21))
		      (+ u v)))

; Looks legit.

; Exercise 5.44 ------------------------------------------------------------------

(define (open-code-primitive? exp env)
  (and (pair? exp)
       (eq? 'not-found (find-variable exp env)) ; NEW
       (memq (operator exp) '(= * - +))))

; Changes the open-coded exercise to use env:

(define (compile exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage env))
        ((quoted? exp) (compile-quoted exp target linkage env))
        ((variable? exp)
         (compile-variable exp target linkage env))
        ((assignment? exp)
         (compile-assignment exp target linkage env))
        ((definition? exp)
         (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   env))
        ((cond? exp) (compile (cond->if exp) target linkage env))
	((open-code-primitive? exp env) ; NEW
	 (dispatch-to-open-code-primitive exp target linkage env)) ; NEW
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (spread-arguments operand-list env)
  (map (lambda (op reg) (compile op reg 'next env))
       operand-list
       '(arg1 arg2)))

(define (compile-open-code-primitive op operands target env)
  (let ((args (spread-arguments operands env)))
    (append-instruction-sequences
     (car args)
     (preserving '(arg1)
		 (cadr args)
		 (make-instruction-sequence
		  '(arg1 arg2)
		  (list target)
		  `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))

(define (dispatch-to-open-code-primitive exp target linkage env)
  (let ((op (operator exp))
	(args (operands exp)))
    (end-with-linkage
     linkage
     (compile-open-code-primitive op (expand-args op args) target env)
     env)))

; End

(compile-and-print '((lambda (+ * a b x y)
		       (+ (* a x) (* b y)))
		     matrix-+ matrix-* 1 2 3 4))

; Section 5.5.7 ------------------------------------------------------------------

(load "~/work/sicp/from_book/ch5-compiler.scm")
(load "~/work/sicp/from_book/load-eceval-compiler.scm")

; Exercise 5.45 ------------------------------------------------------------------

; a.

(define (compile-and-go-factorial)
  (compile-and-go '(define (factorial n)
		     (if (= n 1) 1
			 (* (factorial (- n 1)) n)))))

(with-input-from-string "(factorial 3)" compile-and-go-factorial)
; (total-pushes = 19 maximum-depth = 8)
(with-input-from-string "(factorial 4)" compile-and-go-factorial)
; (total-pushes = 25 maximum-depth = 11)
(with-input-from-string "(factorial 5)" compile-and-go-factorial)
; (total-pushes = 31 maximum-depth = 14)
(with-input-from-string "(factorial 6)" compile-and-go-factorial)
; (total-pushes = 37 maximum-depth = 17)

; Total pushes: 6n+1
; Maximum depth: 3n-1

; Proof:

(map (lambda (n) (+ (* 6 n) 1)) '(3 4 5 6)) ; (19 25 31 37)
(map (lambda (n) (- (* 3 n) 1)) '(3 4 5 6)) ; (8 11 14 17)

; Compiled / iterative:

; Total pushes:

; lim (6n+1)/(16(2n-1))
; n->inf
; Both numerator and denominator approach inf, so this is an indeterminate
; form. Therefore, we can use L'Hopital's rule to determine the limit:
; lim f(x) / g(x) = lim f'(x) / g'(x)
; n->inf            n->inf

; (6n+1)/(16(2n-1))
; = (d/dn)(6n+1)/(d/dn)(16(2n-1))
; = 6/32
; = 0.1875

; Max depth:

; (3n-1)/(5n+3)
; = (d/dn)(3n-1)/(d/dn)(5n+3)
; = 3/5
; = 0.6

; Compiled / special purpose:

; (6n+1)/(2(n-1))
; = (d/dn)(6n+1)/(d/dn)(2(n-1))
; = 6/2
; = 3

; (3n-1)/(2(n-1))
; = (d/dn)(3n-1)/(d/dn)(2(n-1))
; = 3/2
; = 1.5

; Looking at these numbers, it looks like the compiled version of the code
; is significantly more efficient than the interpreted version, and the
; special purpose is also significantly more efficient than the compiled
; version.

; b.

; The special purpose factorial machine doesn't use the environment at all, which
; allows it to be significantly faster. It doesn't need to lookup the variables,
; it just has a dedicated register for each. Some of this cost could be mitigated
; with the compile-environment that we implemented in 5.42, and the open-coded
; primitives in 5.44.

; Exercise 5.46 ------------------------------------------------------------------

; Interpreted version is:
; Max depth = 5n+3.
; Total pushes = 56*fib(n+1)-40.

(define (compile-and-go-fib)
  (compile-and-go '(define (fib n)
		     (if (< n 2) n
			 (+ (fib (- n 1)) (fib (- n 2)))))))

(with-input-from-string "(fib 3)" compile-and-go-fib)
; (total-pushes = 27 maximum-depth = 8)
(with-input-from-string "(fib 4)" compile-and-go-fib)
; (total-pushes = 47 maximum-depth = 11)
(with-input-from-string "(fib 5)" compile-and-go-fib)
; (total-pushes = 77 maximum-depth = 14)
(with-input-from-string "(fib 6)" compile-and-go-fib)
; (total-pushes = 127 maximum-depth = 17)
(with-input-from-string "(fib 7)" compile-and-go-fib)
; (total-pushes = 207 maximum-depth = 20)

; Total pushes:
; 10*fib(n+1)-3

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (stack-compiled n)
  (- (* 10 (fib (+ n 1))) 1))
(map stack-compiled '(3 4 5 6 7)) ; (29 49 79 129 209)

; Max depth:
; 3n-1

(map (lambda (n) (- (* 3 n) 1)) '(3 4 5 6 7)) ; (8 11 14 17 20)

; Compiled / interpreted:

; Max depth: 3n-1/5n+3
; = (d/dn)(3n-1)/(d/dn)(5n+3)
; = 3/5 = 0.60

; Total pushes: S(n)=(10*fib(n+1)-3)/(56*fib(n+1)-40)

(define (stack-interpreted n)
  (- (* 56 (fib (+ n 1))) 40))

(define (ratio n)
  (* 1.0 (/ (stack-compiled n)
	    (stack-interpreted n))))

(map ratio '(3 4 5 6)) ; (.2265625 .20416666666666666 .19362745098039216 .1875)

; It looks like the compiled version is a bit more efficient than the interpreted
; one, but it appears that the difference diminishes as n gets larger.

(define fibonacci-machine
  (make-machine
   '(n continue val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n - 2)
     (assign n (reg val))               ; n now contains Fib(n - 2)
     (restore val)                      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done
     (perform (op print-stack-statistics)))))

(define (run-fib-machine n)
  (set-register-contents! fibonacci-machine 'n n)
  (start fibonacci-machine)
  (get-register-contents fibonacci-machine 'val))

(map run-fib-machine '(3 4 5 6 7 8))
;; (total-pushes = 8 maximum-depth = 4)
;; (total-pushes = 24 maximum-depth = 6)
;; (total-pushes = 52 maximum-depth = 8)
;; (total-pushes = 100 maximum-depth = 10)
;; (total-pushes = 180 maximum-depth = 12)
;; (total-pushes = 312 maximum-depth = 14)

; Max depth: 2n-2
; Total pushes: 2*fib(n+1) 
