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

; Exercise 5.10 -------------------------------------------------------------------

