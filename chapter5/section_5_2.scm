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

