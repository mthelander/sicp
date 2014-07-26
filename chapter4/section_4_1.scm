; Section 4.1.1 ------------------------------------------------------------------------------

; Exercise 4.1 -------------------------------------------------------------------------------

(define (eval x e) x)
(define no-operands? null?)

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (car exps) env)
            (list-of-values-left-to-right (cdr exps) env))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? (cdr exps))
      (car exps)
      (cons (list-of-values-right-to-left (cdr exps) env)
	    (eval (car exps) env))))

(list-of-values-left-to-right '(a b c d) '()) ; (a b c d)
(list-of-values-right-to-left '(a b c d) '()) ; (((d . c) . b) . a)

; Section 4.1.2 ------------------------------------------------------------------------------

; Exercise 4.2 -------------------------------------------------------------------------------

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; a)
;
; Louis Reasoner is wrong again! He's wrong because (application?) is too inclusive in its
; definition of acceptable expressions. If we pass it (define x 3) then it will try to apply
; the procedure define to the arguments (x 3).


; b)

(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

; Exercise 4.3 -------------------------------------------------------------------------------

; test scaffolding
(define put 2d-put!)
(define get 2d-get)
(define (true? x)
  (not (or (eq? x 'false) (null? x))))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
; end test scaffolding

(define (install-eval-package)
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
						       (lambda-body exp)
						       env)))
  (put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'eval (car exp))
	 ((get 'eval (car exp)) exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(install-eval-package)
(eval '(if 3 4 5) '())

; Exercise 4.4 -------------------------------------------------------------------------------

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; new code

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (if (null? exp)
      'false
      (let ((val (eval (first-exp exp) env)))
	(if (true? val)
	    (if (last-exp? exp)
		val
		(eval-and (rest-exps exp) env))
	    'false))))

(define (eval-or exp env)
  (if (null? exp)
      'false
      (let ((val (eval (first-exp exp) env)))
	(if (true? val)
	    val
	    (eval-or (rest-exps exp) env)))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
	((and? exp) (eval-and (operands exp) env))
	((or? exp) (eval-or (operands exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(eval '(and 3 4) '()) ; 4
(eval '(and '() 5) '()) ; false

(eval '(or '() 5) '()) ; 5
(eval '(or 3 4) '()) ; 3

; Exercise 4.5 -------------------------------------------------------------------------------

(define (cond-actions clause)
  (if (eq? (cadr clause) '=>)
      (caddr clause)
      (cdr clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    (let ((predicate (cond-predicate first))
		  (actions (cond-actions first)))
	      (make-if predicate
		       (if (pair? actions)
			   (sequence->exp actions)
			   (list actions predicate))
		       (expand-clauses rest)))))))

(cond->if '(cond ((even? 3) 5)
		 (else 10)))
; (if (even? 3) 5 10)

(cond->if '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
		 (else false)))
; (if (assoc (quote b) (quote ((a 1) (b 2))))
;     (cadr (assoc (quote b) (quote ((a 1) (b 2)))))
;     (else false))

; Exercise 4.6 -------------------------------------------------------------------------------

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-pairs exp) (cadr exp))
(define (let-vars exp) (map car (let-pairs exp)))
(define (let-values exp) (map cadr (let-pairs exp)))

(define test-let '(let ((foo 3)
			(bar 5)
			(baz 8))
		    (+ foo bar baz)))

(let-body test-let) ; (+ foo bar baz)
(let-vars test-let) ; (foo bar baz)
(let-values test-let) ; (3 5 8)

(define (let->combination exp)
  (cons (cons 'lambda
	      (cons (let-vars exp)
		    (let-body exp)))
	(let-values exp)))

(let->combination test-let) ; ((lambda (foo bar baz) (+ foo bar baz)) 3 5 8)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
	((and? exp) (eval-and (operands exp) env))
	((or? exp) (eval-or (operands exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
	((let? exp) (eval (let->combination exp) env)) ; <-- new code
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


; Exercise 4.7 -------------------------------------------------------------------------------

; We just have to transform each let variable to be a new let combination which wraps the
; subsequent let variables (also transformed into combinations), with the innermost
; combination containing the let body.

; It is sufficient to simply add (eval (let*->nested-lets exp) env) to eval (and an
; appropriate predicate),

(define (let*->nested-lets exp)
  (define (iter args)
    (list 'let (list (first-exp args))
	  (if (last-exp? args)
	      (let-body exp)
	      (iter (rest-exps args)))))
  (iter (let-pairs exp)))

(let*->nested-lets '(let* ((x 3)
			   (y (+ x 2))
			   (z (+ x y 5)))
		      (* x z)))
; (let ((x 3))
;   (let ((y (+ x 2)))
;     (let ((z (+ x y 5)))
;       (* x z))))

; Exercise 4.8 -------------------------------------------------------------------------------

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-pairs exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-vars exp) (map car (let-pairs exp)))
(define (let-values exp) (map cadr (let-pairs exp)))
(define (let-name exp)
  (if (named-let? exp)
      (cadr exp)
      '()))
(define (named-let? exp)
  (not (pair? (cadr exp))))

(define (named-let->let exp)
  (list 'let (let-pairs exp)
	(cons 'define
	      (cons (cons (let-name exp)
			  (let-vars exp))
		    (let-body exp)))
	(cons (let-name exp) (let-values exp))))

(define (let->combination exp)
  (if (named-let? exp)
      (let->combination (named-let->let exp))
      (cons (cons 'lambda
		  (cons (let-vars exp)
			(let-body exp)))
	    (let-values exp))))

; tests
(define test-let '(let ((foo 3)
			(bar 5)
			(baz 8))
		    (+ foo bar baz)))

(define test-named-let '(let fib-iter ((a 1)
				       (b 0)
				       (count 3))
			  (if (= count 0)
			      b
			      (fib-iter (+ a b) a (- count 1)))))

(let-body test-let) ; (+ foo bar baz)
(let-vars test-let) ; (foo bar baz)
(let-values test-let) ; (3 5 8)
(let-name test-let) ; ()

(let-body test-named-let) ; (if (= count 0) b (fib-iter (+ a b) a (- count 1)))
(let-vars test-named-let) ; (a b count)
(let-values test-named-let) ; (1 0 3)
(let-name test-named-let) ; fib-iter

(let->combination test-let) ; ((lambda (foo bar baz) (+ foo bar baz)) 3 5 8)
(let->combination test-named-let)
;; ((lambda (a b count)
;;    (define (fib-iter a b count)
;;      (if (= count 0) b
;; 	 (fib-iter (+ a b) a (- count 1))))
;;    (fib-iter 1 0 3))
;;  1 0 3)
; end tests

; Exercise 4.9 -------------------------------------------------------------------------------

; We can define a looping construct in terms of a procedure that calls itself repeatedly.

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (while->combination exp)
  (list 'define (list 'while)
	(list 'cond (list (while-predicate exp)
			  (cons 'begin (while-body exp))
			  (list 'while))
	      (list 'else (list 'while)))))

(while->combination '(while (> i 1)
			    (display "foobar")
			    (newline)
			    (- i 1)))
;; ;; Output
;; (define (while)
;;   (cond ((> i 1)
;; 	 (begin (display "foobar")
;; 		(newline)
;; 		(- i 1))
;; 	 (while))
;; 	(else (while))))

;; Add the following line to eval:
; ((while? exp) (eval (while->combination exp) env))

; Exercise 4.10 ------------------------------------------------------------------------------

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (cadr exp) tag)
      false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (car exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons parameters (cons 'lambda body)))

(define (make-procedure params body env)
  (list params body)) ; Don't really do anything, just test it.

(eval '((a b) lambda (+ a b)) '()) ; ((a b) ((+ a b)))

; Section 4.1.3 ------------------------------------------------------------------------------

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Exercise 4.11 ------------------------------------------------------------------------------

(define (make-frame variables values)
  (if (null? variables) '()
      (cons (cons (car variables) (car values))
	    (make-frame (cdr variables) (cdr values)))))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define test-frame (make-frame '(foo bar baz) '(5 8 13))) ; ((foo 5) (bar 8) (baz 13))
(define test-env (list test-frame))

(frame-variables test-frame) ; (foo bar baz)
(frame-values test-frame) ; (5 8 13)
(add-binding-to-frame! 'bak 21 test-frame)
(car test-frame) ; (bak 21)
(car (cdr test-frame)) ; (foo 5)
(cdr test-frame) ; ((foo 5) (bar 8) (baz 13))

(lookup-variable-value 'baz test-env) ; 13

; Exercise 4.12 ------------------------------------------------------------------------------

(define (find-where predicate env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((predicate (car bindings))
             (car bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment) '()
	(scan (first-frame env))))
  (env-loop env))

(define (find-where-or-die predicate env)
  (let ((binding (find-where predicate env)))
    (if (null? binding)
	(error "Unbound variable")
	binding)))

(define (eq-var? var)
  (lambda (x) (eq? (car x) var)))

(define (lookup-variable-value var env)
  (cdr (find-where-or-die (eq-var? var) env)))
(define (set-variable-value! var val env)
  (set-cdr! (find-where-or-die (eq-var? var) env) val))
(define (define-variable! var val env)
  (let ((binding (find-where (eq-var? var) env)))
    (if (null? binding)
	(add-binding-to-frame! var val (first-frame env))
	(set-cdr! binding val))))

(lookup-variable-value 'baz test-env) ; 13

(set-variable-value! 'baz 34 test-env)
(lookup-variable-value 'baz test-env) ; 34

(define-variable! 'baz 55 test-env)
(lookup-variable-value 'baz test-env) ; 55
(define-variable! 'qux 89 test-env)
(lookup-variable-value 'qux test-env) ; 89

; Exercise 4.13 ------------------------------------------------------------------------------

(define (make-unbound! var env)
  (define (remove-var frames)
    (cond ((null? frames) '())
	  ((eq? (car (first-frame frames)) var)
	   (remove-var (cdr frames)))
	  (else (cons (first-frame frames)
		      (remove-var (cdr frames))))))
  (map! remove-var env))

(make-unbound! 'baz (list (make-frame '(foo bar baz) '(2 3 5))
			  (make-frame '(foo bar baz) '(8 13 21))
			  (make-frame '(foo bar baz) '(34 55 89))))

; (((foo . 2) (bar . 3)) ((foo . 8) (bar . 13)) ((foo . 34) (bar . 55)))

; Section 4.1.4 ------------------------------------------------------------------------------

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;<more primitives>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

; Exercise 4.14 ------------------------------------------------------------------------------

; When Eva defines map, it gets routed through eval, and thus is created as a compound
; procedure (a tagged list). Louis's would work if he wrapped his primitive definition in
; call to make-procedure.

; Exercise 4.15 ------------------------------------------------------------------------------

; The halts? procedure cannot be implemented, because it's existence would create a paradox
; illustrated by the following scenario:

;; (define (run-forever) (run-forever))
;; (define (try p)
;;   (if (halts? p p)
;;       (run-forever)
;;       'halted))
;; (try try)

; If halts? returns true for (try try) then the result would be false, however, that would
; mean that it returns true, which means it returns false... resulting in an infinite
; "flip-flop" of return values. This is a variation of the liar paradox.

; Exercise 4.16 ------------------------------------------------------------------------------

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (procedure-body p) (cddr p))

; a.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
		 (error "Unassigned value -- " vars)
		 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Tests
(define test-frame (make-frame '(foo bar baz bak)
			       '(5 8 13 *unassigned*)))
(define test-env (list test-frame))

;(lookup-variable-value 'bak test-env) ; Unassigned value -- (bak)

; b.

(define (scan-out-defines body)
  (let ((defines (filter definition? body)))
    (cons 'let
	  (cons (map (lambda (d) (list (definition-variable d) '*unassigned*)) defines)
		(cons (cons 'begin (map (lambda (d) (list 'set!
							(definition-variable d)
							(definition-value d)))
				      defines))
		      (filter (lambda (d) (not (definition? d))) body))))))

(scan-out-defines '((define u (+ 1 2 3))
		    (define v (* 1 2 3))
		    (+ u v)))

;; (let ((u *unassigned*)
;;       (v *unassigned*))
;;   (begin (set! u (+ 1 2 3))
;;        (set! v (* 1 2 3)))
;;   (+ u v))

; c.

; I think it's better to install it in make-procedure, that way the translation only happens
; once, whereas it would happen everytime a procedure call is evaluated (via apply) if it
; were in procedure-body.

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; Exercise 4.17 ------------------------------------------------------------------------------

; Untransformed environment:

;                     E1             E2
;                     +---------+    +------+
; [enclosing env] <---| <vars>  |<---| <e3> |
;                     | u: <e1> |    +------+
;                     | v: <e2> |
;                     +---------+


; Transformed environment:

;                     E1             E2
;                     +------+    +---------+    +------+
; [enclosing env] <---|<vars>|<---| u: <e1> |<---| <e3> |
;                     +------+    | v: <e2> |    +------+
;                                 +---------+

; There's an extra frame in the transformed version because, as we know from exercise 4.6, let
; is syntactic-sugar for an immediately-invoked lambda. This won't ever change the semantics
; of our program because the "extra" frame contains the entirety of the environment for the
; procedure, with the enclosing frame empty. I think tail-call-optimization will optimize this
; away for us anyway.

; We could avoid this extra frame by simply placing the defines at the beginning of the
; procedure evaluation:

;; (define (scan-out-defines body)
;;   (let ((defines (filter definition? body))
;; 	(not-defines (filter (lambda (d) (not (definition? d))) body)))
;; 	(cons 'begin (append defines not-defines))))

;; (scan-out-defines '((define u (+ 1 2 3))
;; 		    (+ u v)
;; 		    (define v (* 1 2 3))))

;; ;(begin (define u (+ 1 2 3))
;; ;     (define v (* 1 2 3))
;; ;     (+ u v))

; Exercise 4.18 ------------------------------------------------------------------------------

; No, this will NOT work because the contents of <e1> & <e2> in this case contain references to
; other. Specifically, dy references y when it is first evaluated. It depends on the fact that
; y is initialized first, which wouldn't be the case if we store the values in temporary
; variables first. The implementation described in exercise 4.16 however would be correct,
; because the describes are initialized in the correct order.

; Exercise 4.19 ------------------------------------------------------------------------------

; I think Ben is wrong, and either Alyssa or Eva's viewpoints are correct. We could implement
; Eva's preference by ordering the define statements so that defines that reference others are
; ordered after the ones that do not. However, I think that would only work for the most
; contrived cases.

; Exercise 4.20 ------------------------------------------------------------------------------

; a.

;; '(define (f x)
;;   (letrec ((even?
;;             (lambda (n)
;;               (if (= n 0)
;;                   true
;;                   (odd? (- n 1)))))
;;            (odd?
;;             (lambda (n)
;;               (if (= n 0)
;;                   false
;;                   (even? (- n 1))))))
;;     "<rest of body of f>"))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-body exp) (cddr exp))
(define (letrec-pairs exp) (cadr exp))
(define (letrec-vars exp) (map car (let-pairs exp)))
(define (letrec-values exp) (map cadr (let-pairs exp)))

(define (make-let vars body)
  (cons 'let (list vars body)))
(define (make-begin exp)
  (cons 'begin exp))

(define (letrec->combination exp)
  (make-let (map (lambda (v) (list v '*unassigned*)) (letrec-vars exp))
	    (make-begin (append
			 (map (lambda (pair) (cons 'set! pair)) (letrec-pairs exp))
			 (letrec-body exp)))))

(letrec->combination '(letrec ((fact
				(lambda (n)
				  (if (= n 1)
				      1
				      (* n (fact (- n 1)))))))
			(fact 10)))
;; (let ((fact *unassigned*))
;;   (begin (set! fact (lambda (n) (if (= n 1)
;; 				    1
;; 				    (* n (fact (- n 1))))))
;;        (fact 10)))

(letrec->combination '(letrec ((even?
				(lambda (n)
				  (if (= n 0)
				      true
				      (odd? (- n 1)))))
			       (odd?
				(lambda (n)
				  (if (= n 0)
				      false
				      (even? (- n 1))))))
			"<rest of body of f>"))
;; (let ((even? *unassigned*)
;;       (odd? *unassigned*))
;;   (begin (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
;; 	 (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
;; 	 "<rest of body of f>"))

; b.

; SKIPPED

; Exercise 4.21 ------------------------------------------------------------------------------

; a.

(define factorial (lambda (n)
		    ((lambda (fact) (fact fact n))
		     (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))))

(factorial 10) ; 3628800
(factorial 4)  ; 24
(factorial 6)  ; 720

; It really does compute factorials.

(define fib (lambda (n)
	      ((lambda (fib) (fib fib n))
	       (lambda (ft k) (if (< k 2) k (+ (ft ft (- k 1)) (ft ft (- k 2))))))))

(fib 6)  ; 8
(fib 9)  ; 34
(fib 15) ; 610

; b.

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n) (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n) (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 3)  ; #f
(f 9)  ; #f
(f 12) ; #t

; Section 4.1.7 ------------------------------------------------------------------------------

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

; Exercise 4.22 ------------------------------------------------------------------------------

;; (define (analyze exp)
;;   (cond ((self-evaluating? exp) 
;;          (analyze-self-evaluating exp))
;;         ((quoted? exp) (analyze-quoted exp))
;;         ((variable? exp) (analyze-variable exp))
;;         ((assignment? exp) (analyze-assignment exp))
;;         ((definition? exp) (analyze-definition exp))
;;         ((if? exp) (analyze-if exp))
;;         ((lambda? exp) (analyze-lambda exp))
;;         ((begin? exp) (analyze-sequence (begin-actions exp)))
;; 	((let? exp) (analyze (let->combination exp))) ; <-- new code
;;         ((cond? exp) (analyze (cond->if exp)))
;;         ((application? exp) (analyze-application exp))
;;         (else
;;          (error "Unknown expression type -- ANALYZE" exp))))

; LOST

; Exercise 4.23 ------------------------------------------------------------------------------

; LOST
