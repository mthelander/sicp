; Section 5.3.1-------------------------------------------------------------------

(load "~/work/sicp/from_book/ch5-regsim.scm")

; Exercise 5.20 ------------------------------------------------------------------

;; (define x (cons 1 2))
;; (define y (list x x))

;; x: (1 2) --> (|)-->(2)
;;             1 |
;;               v
;;              (1)

;; y: (x x) --> (|)--------->(|)-->x
;;              2|           3|
;;               +------------+
;;               |
;;               |
;;               v
;;   	        (|)-->(2)
;;             1 |
;; 	         v
;; 	        (1)

;;    Index  0  1  2  3  4  5  6  7  8  ...
;;          +-------------------------------
;; the-cars |  |n1|p1|p1|  |  |  |  |  | ...
;;          +-------------------------------
;; the-cdrs |  |n2|p3|e0|  |  |  |  |  | ...
;;          +-------------------------------

;; At the end of this, free will be 4. x is represented by the pointer p1, and y
;; is represented by the pointer p2.

; Exercise 5.21 ------------------------------------------------------------------

; a.

(define count-leaves-machine
  (make-machine
   '(tree val continue temp)
   `((+ ,+)
     (null? ,null?)
     (pair? ,pair?)
     (car ,car)
     (cdr ,cdr)
     (display ,display))
   '(start
     (assign continue (label end))

    count-leaves
     (test (op null?) (reg tree))
     (branch (label null-tree))

     (test (op pair?) (reg tree))
     (branch (label pair))
     (goto (label non-pair))

    null-tree
     (assign val (const 0))
     (goto (reg continue))

    non-pair
     (assign val (const 1))
     (goto (reg continue))

    pair
     (save continue)
     (assign continue (label after-car))
     (save tree)
     (assign tree (op car) (reg tree))
     (goto (label count-leaves))

    after-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (save temp)
     (assign temp (reg val))
     (goto (label count-leaves))

    after-cdr
     (assign val (op +) (reg val) (reg temp))
     (restore temp)
     (restore continue)
     (goto (reg continue))

    end)))

(set-register-contents! count-leaves-machine 'tree '((3 4) (5 6 7 (8))))
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val) ; 6

; b.

(define count-leaves-machine
  (make-machine
   '(tree val continue n)
   `((+ ,+)
     (null? ,null?)
     (pair? ,pair?)
     (car ,car)
     (cdr ,cdr)
     (newline ,newline)
     (display ,display))
   '(start
      (assign n (const 0))
      (assign continue (label end))

     count-leaves
      (test (op null?) (reg tree))
      (branch (label null-tree))
      (test (op pair?) (reg tree))
      (branch (label pair))
      (goto (label non-pair))

     null-tree
      (assign val (reg n))
      (goto (reg continue))

     non-pair
      (assign val (op +) (reg n) (const 1))
      (goto (reg continue))

     pair
      (save continue)
      (assign continue (label after-car))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label count-leaves))

     after-car
      (restore tree)
      (restore continue)
      (assign tree (op cdr) (reg tree))
      (assign n (reg val))
      (goto (label count-leaves))

     end)))

(set-register-contents! count-leaves-machine 'tree '((3 4) (5 6 7 (8))))
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val) ; 6

; Exercise 5.22 ------------------------------------------------------------------

(define append-machine
  (make-machine
   '(x y val temp continue)
   `((null? ,null?)
     (car ,car)
     (cdr ,cdr)
     (cons ,cons))
   '(start
      (assign continue (label end))
      (assign temp (const ()))

     append-loop
      (test (op null?) (reg x))
      (branch (label null-x))
      (save temp)
      (assign temp (op car) (reg x))
      (assign x (op cdr) (reg x))
      (save continue)
      (assign continue (label cons-x-to-y))
      (goto (label append-loop))

     null-x
      (assign val (reg y))
      (goto (reg continue))

     cons-x-to-y
      (assign val (op cons) (reg temp) (reg val))
      (restore continue)
      (restore temp)
      (goto (reg continue))

     end)))

(set-register-contents! append-machine 'x '(a b c d e))
(set-register-contents! append-machine 'y '(1 2 3))
(start append-machine)
(get-register-contents append-machine 'val) ; (a b c d e 1 2 3)

(define append!-machine
  (make-machine
   '(x y temp val)
   `((null? ,null?)
     (cdr ,cdr)
     (set-cdr! ,set-cdr!))
   '(start
      (assign temp (reg x))

      last-pair
       (save temp)
       (assign temp (op cdr) (reg temp))
       (test (op null?) (reg temp))
       (branch (label end-of-last-pair))
       (goto (label last-pair))

      end-of-last-pair
       (restore temp)
       (perform (op set-cdr!) (reg temp) (reg y))
       (assign val (reg x))
       
     end)))

(set-register-contents! append!-machine 'x '(a b c d e))
(set-register-contents! append!-machine 'y '(1 2 3))
(start append!-machine)
(get-register-contents append!-machine 'val) ; (a b c d e 1 2 3)
(get-register-contents append!-machine 'x) ; (a b c d e 1 2 3)
