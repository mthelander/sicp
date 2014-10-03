(load "~/work/sicp/from_book/ch5-regsim.scm")

; Exercise 5.7 -------------------------------------------------------------------

(define rec-expt-machine
  (make-machine
   '(b n continue val)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
       (assign continue (label expt-done))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
     after-expt
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))
     base-case
       (assign val (const 1))         
       (goto (reg continue))
     expt-done)))

(set-register-contents! rec-expt-machine 'b 2)
(set-register-contents! rec-expt-machine 'n 12)
(start rec-expt-machine)
(get-register-contents rec-expt-machine 'val) ; 4096

(set-register-contents! rec-expt-machine 'b 9)
(set-register-contents! rec-expt-machine 'n 3)
(start rec-expt-machine)
(get-register-contents rec-expt-machine 'val) ; 729

(define iter-expt-machine
  (make-machine
   '(b n counter product)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
      (assign counter (reg n))
      (assign product (const 1))
     expt-iter
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg product) (reg b))
      (goto (label expt-iter))
     expt-done)))

(set-register-contents! iter-expt-machine 'b 2)
(set-register-contents! iter-expt-machine 'n 12)
(start iter-expt-machine)
(get-register-contents iter-expt-machine 'product) ; 4096

(set-register-contents! iter-expt-machine 'b 9)
(set-register-contents! iter-expt-machine 'n 3)
(start iter-expt-machine)
(get-register-contents iter-expt-machine 'product) ; 729

; Exercise 5.8 -------------------------------------------------------------------

; Register a will contain the value 3.

;; (define (extract-labels text receive)
;;   (if (null? text)
;;       (receive '() '())
;;       (extract-labels (cdr text)
;;        (lambda (insts labels)
;;          (let ((next-inst (car text)))
;;            (if (symbol? next-inst)
;; 	       (if (assoc next-inst labels)
;; 		   (error "Label is multiply defined --" next-inst)
;; 		   (receive insts
;; 			    (cons (make-label-entry next-inst
;; 						    insts)
;; 				  labels)))
;;                (receive (cons (make-instruction next-inst)
;;                               insts)
;;                         labels)))))))

;; (define test-foo
;;   (make-machine
;;    '(a)
;;    '()
;;    '(start
;;        (goto (label here))
;;      here
;;        (assign a (const 3))
;;        (goto (label there))
;;      here
;;        (assign a (const 4))
;;        (goto (label there))
;;      there)))

;; (start test-foo)
;; (get-register-contents test-foo 'a) ; Error: Label is multiply defined -- here

; Exercise 5.9 -------------------------------------------------------------------

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
		(case (car e)
		  ((const reg) (make-primitive-exp e machine labels))
		  (else (error "Invalid operation argument --" e))))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; (define test-machine
;;   (make-machine
;;    '(output)
;;    (list (list 'do-stuff (lambda (a b) 1223)))
;;    '(start
;;        (assign output (op do-stuff) (const 3) (label end))
;;      end)))

;; (start test-machine)
;; (get-register-contents test-machine 'output)
;Invalid operation argument -- (label end)

; Exercise 5.10 ------------------------------------------------------------------

;; (define (last-pair list)
;;   (if (null? (cdr list))
;;       (car list)
;;       (last-pair (cdr list))))

;; (define (front-pairs list)
;;   (if (null? (cdr list)) '()
;;       (cons (car list)
;; 	    (front-pairs (cdr list)))))

;; (define (operation-exp? exp)
;;   (and (pair? exp)
;;        (tagged-list? (last-pair exp) 'op)))

;; (define (operation-exp-op operation-exp)
;;   (cadr (last-pair operation-exp)))

;; (define (operation-exp-operands operation-exp)
;;   (front-pairs operation-exp))

;; (define test-machine
;;   (make-machine
;;    '(output)
;;    (list (list '* *))
;;    '(controller
;;        (assign output (const 9) (const 8) (op *))
;;     done)))

;; (start test-machine)
;; (get-register-contents test-machine 'output) ; 72

; Exercise 5.11 ------------------------------------------------------------------

; a.

(define fibonacci-machine
  (make-machine
   '(continue n val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
    fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
    afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
    afterfib-n-2
     ; START OF CHANGE
     ;(assign n (reg val))
     ;(restore val)
     (restore n)
     ; /CHANGE
     (restore continue)
     (assign val
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))
    immediate-answer
     (assign val (reg n))
     (goto (reg continue))
    fib-done)))

(set-register-contents! fibonacci-machine 'n 8)
(start fibonacci-machine)
(get-register-contents fibonacci-machine 'val) ; 21

; b.

;; (define (make-save inst machine stack pc)
;;   (let* ((reg-name (stack-inst-reg-name inst))
;; 	 (reg (get-register machine reg-name)))
;;     (lambda ()
;;       (push stack (list reg-name (get-contents reg)))
;;       (advance-pc pc))))

;; (define (make-restore inst machine stack pc)
;;   (let* ((reg-name (stack-inst-reg-name inst))
;; 	 (reg (get-register machine reg-name)))
;;     (lambda ()
;;       (let ((top-of-stack (pop stack)))
;; 	(if (not (eq? (car top-of-stack) reg-name))
;; 	    (error "Tried to restore an invalid register --"
;; 		   reg-name
;; 		   (car top-of-stack))
;; 	    (begin
;; 	      (set-contents! reg (cadr top-of-stack))
;; 	      (advance-pc pc)))))))

;; (define test-machine
;;   (make-machine
;;    '(x y)
;;    '()
;;    '(start
;;      (assign x (const 5))
;;      (assign y (const 8))
;;      (save x)
;;      (save y)
;;      (restore x)
;;     end)))

;; (start test-machine)
;; ;Tried to restore an invalid register -- x y

; c.

;; (define (make-stack)
;;   (let ((stack-list '()))
;;     (define (push name x)
;;       (let ((s (assoc name stack-list)))
;; 	(if s
;; 	    (set! s (cons x s))
;; 	    (set! stack-list (cons (list name x)
;; 				   stack-list)))))
;;     (define (pop name)
;;       (let ((s (assoc name stack-list)))
;; 	(if (or (not s) (null? (cdr s)))
;; 	    (error "Empty stack -- POP")
;; 	    (let ((top (car (cdr s))))
;; 	      (set! s (cdr s))
;; 	      top))))
;;     (define (initialize)
;;       (set! stack-list '())
;;       'done)
;;     (define (dispatch message)
;;       (cond ((eq? message 'push) push)
;;             ((eq? message 'pop) pop)
;;             ((eq? message 'initialize) (initialize))
;;             (else (error "Unknown request -- STACK"
;;                          message))))
;;     dispatch))

;; (define (pop reg-name stack)
;;   ((stack 'pop) reg-name))

;; (define (push stack reg-name value)
;;   ((stack 'push) reg-name value))

;; (define (make-save inst machine stack pc)
;;   (let ((reg (get-register machine
;;                            (stack-inst-reg-name inst))))
;;     (lambda ()
;;       (push stack (stack-inst-reg-name inst) (get-contents reg))
;;       (advance-pc pc))))

;; (define (make-restore inst machine stack pc)
;;   (let ((reg (get-register machine
;;                            (stack-inst-reg-name inst))))
;;     (lambda ()
;;       (set-contents! reg (pop (stack-inst-reg-name inst) stack))
;;       (advance-pc pc))))

;; (define test-machine
;;   (make-machine
;;    '(x y)
;;    '()
;;    '(start
;;      (assign x (const 5))
;;      (assign y (const 8))
;;      (save x)
;;      (save y)
;;      (restore x)
;;     end)))

;; (start test-machine)
;; (get-register-contents test-machine 'x) ; 5

; Exercise 5.12 ------------------------------------------------------------------

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(sorted-instructions '()) ; NEW
	(holding-registers '()) ; NEW
	(stack-registers '()) ; NEW
	(source-vals-by-register '())) ; NEW
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(save-instruction (instruction-text (car insts))) ; NEW
                ((instruction-execution-proc (car insts)))
                (execute)))))
      ; NEW BLOCK OF CODE
      (define (save-instruction inst-text) ; NEW
	(save-sorted-instruction inst-text)
	(case (car inst-text)
	  ((assign)
	   (save-holding-register (assign-reg-name inst-text))
	   (save-source-register (assign-reg-name inst-text)
				 (assign-value-exp inst-text)))
	  ((save restore)
	   (save-stack-register (stack-inst-reg-name inst-text)))))
      (define (save-sorted-instruction inst-text)
	(set! sorted-instructions
	      (add-list-to-list inst-text sorted-instructions)))
      (define (save-holding-register reg-name)
	(set! holding-registers
	      (add-symbol-to-list reg-name holding-registers)))
      (define (save-stack-register reg-name)
	(set! stack-registers
	      (add-symbol-to-list reg-name stack-registers)))
      (define (save-source-register reg-name reg-value)
	(let ((stored-vals (assoc reg-name source-vals-by-register)))
	  (if stored-vals
	      (set! source-vals-by-register
		    (add-to-list (cons (car stored-vals)
				       (add-to-list reg-value
						    (cdr stored-vals)
						    (lambda (a b) true)))
				 (del-assoc reg-name source-vals-by-register)
				 (lambda (a b) true)))
	      (set! source-vals-by-register
		    (cons (list reg-name reg-value)
			  source-vals-by-register)))))
      (define (add-list-to-list el list)
	(add-to-list el list (lambda (a b) (symbol<? (car a) (car b)))))
      (define (add-symbol-to-list el list)
	(add-to-list el list (lambda (a b) (symbol<? a b))))
      (define (add-to-list el list sort-proc)
	(if (memq el list)
	    list
	    (sort (cons el list) sort-proc)))
      ; END OF NEW CODE
      (define (dispatch message)
        (case message
	  ((start)
	   (set-contents! pc the-instruction-sequence)
	   (execute))
	  ((install-instruction-sequence)
	   (lambda (seq) (set! the-instruction-sequence seq)))
	  ((allocate-register) allocate-register)
	  ((get-register) lookup-register)
	  ((install-operations)
	   (lambda (ops) (set! the-ops (append the-ops ops))))
	  ((stack) stack)
	  ((operations) the-ops)
	  ((get-instructions) sorted-instructions) ; NEW
	  ((get-holding-registers) holding-registers) ; NEW
	  ((get-stack-registers) stack-registers) ; NEW
	  ((get-source-values) source-vals-by-register) ; NEW
	  (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define fibonacci-machine
  (make-machine
   '(continue n val)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
    fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
    afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
    afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))
    immediate-answer
     (assign val (reg n))
     (goto (reg continue))
    fib-done)))

(set-register-contents! fibonacci-machine 'n 8)
(start fibonacci-machine)
(fibonacci-machine 'get-instructions)
;; ((assign val (op +) (reg val) (reg n))
;;  (assign n (reg val))
;;  (assign continue (label afterfib-n-2))
;;  (assign n (op -) (reg n) (const 2))
;;  (assign val (reg n))
;;  (assign n (op -) (reg n) (const 1))
;;  (assign continue (label afterfib-n-1))
;;  (assign continue (label fib-done))
;;  (branch (label immediate-answer))
;;  (goto (reg continue))
;;  (goto (label fib-loop))
;;  (goto (reg continue))
;;  (goto (label fib-loop))
;;  (restore continue)
;;  (restore val)
;;  (restore continue)
;;  (restore n)
;;  (save val)
;;  (save continue)
;;  (save n)
;;  (save continue)
;;  (test (op <) (reg n) (const 2)))
(fibonacci-machine 'get-holding-registers)
;; (continue n val)
(fibonacci-machine 'get-stack-registers)
;; (continue n val)
(fibonacci-machine 'get-source-values)
;; ((n ((op -) (reg n) (const 2))
;;     ((op -) (reg n) (const 1))
;;     ((reg val)))
;;  (continue ((label afterfib-n-1))
;; 	   ((label fib-done))
;; 	   ((label afterfib-n-2)))
;;  (val ((reg n))
;;       ((op +) (reg val) (reg n))))

; Exercise 5.13 ------------------------------------------------------------------

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name) ; NEW CODE
	      	     (lookup-register name))))) ; NEW CODE
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (case message
	  ((start)
	   (set-contents! pc the-instruction-sequence)
	   (execute))
	  ((install-instruction-sequence)
	   (lambda (seq) (set! the-instruction-sequence seq)))
	  ((allocate-register) allocate-register)
	  ((get-register) lookup-register)
	  ((install-operations)
	   (lambda (ops) (set! the-ops (append the-ops ops))))
	  ((stack) stack)
	  ((operations) the-ops)
	  (else (error "Unknown request -- MACHINE" message))))

      dispatch)))

(define fibonacci-machine
  (make-machine
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
     (assign continue (label fib-done))
    fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
    afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
    afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))
    immediate-answer
     (assign val (reg n))
     (goto (reg continue))
    fib-done)))

(set-register-contents! fibonacci-machine 'n 8)
(start fibonacci-machine)
(get-register-contents fibonacci-machine 'n) ; 8
