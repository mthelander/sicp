; Exercise 5.1 ------------------------------------------------------------------

;;                                   ____            +---+
;;                                  (  > )<----------+ n |
;;                                   ----            +---+
;;                                    |                        
;;                                    |                        
;;                                    |                        
;;                                    |                        
;;      +---------+                +----+----+                   
;; +--->| product |            +---+ counter |<---+
;; |    +---+-----+            |   +-----+---+    |            
;; |        |                  |       |          x c<-c
;; x p<-p   |                  |       |          |            
;; |        |                  |       |          |            
;; |        |                  |       |          |            
;; |        |      +-------+   |      ___         |            
;; |        +----->+  *    |<--+     \ + /--------+
;; |               +---+---+          ---
;; |                   |              ^                         
;; +-------------------+              |                         
;;                                    |                         
;;                                    +
;;                                   / \
;;                                  / 1 \
;;                                 +-----+  

;;           start             
;;             +               
;;             |               
;;         +---+---+  yes      
;; +------>|   >   +------> done
;; |       +---+---+           
;; |           |no             
;; |           |               
;; |       +---+---+           
;; |       | p<-p  |           
;; |       +---+---+           
;; |           |                
;; |           |                
;; |           |               
;; |       +---+---+           
;; +-------+ c<-c  |           
;;         +-------+           

; Exercise 5.2 ------------------------------------------------------------------

;; (controller
;;    (assign product (const 1))
;;    (assign counter (const 1))
;;  test-factorial
;;    (test (op >) (reg counter) (const n))
;;    (branch (label factorial-done))
;;    (assign product (op *) (reg product) (reg counter))
;;    (assign counter (op +) (reg counter) (const 1))
;;    (goto (label test-factorial))
;;  factorial-done)

; Exercise 5.3 ------------------------------------------------------------------

;; With only primitives:

;;        +-------+
;;    +---+ guess |-------> (good-enough?)
;;    |   +-------+
;;    |         x guess<-guess
;;   +-------+  |
;;  / improve \-+
;; +-----------+

;; (controller
;;    (assign guess (const 1.0))
;;  test-sqrt
;;    (test (op good-enough) (reg guess))
;;    (branch (label sqrt-done))
;;    (assign guess (op improve) (reg guess))
;;    (goto (label test-sqrt))
;;  sqrt-done)

;; Without primitives:
;;  e->guess                guess->b
;;   |     +-----+    +------+ |  +-+   +-----+
;;   +x---+|guess|--->|square|-x->|b|-->|  -  |----+
;;   | +---+-----+    +------+    +-+   +-+---+    x - b->c
;;   | |      +----x----+                 |        |
;; +---++   +---+  |   +-+-+             +---+    +-+-+  
;; |avg +---| e +--+---| / |<------------| x |    | c |  
;; +----+   +---+  |   +---+             +---+    +---+  
;;                 |                               |
;;               guess->e                        +-+-+ 
;;                                               |abs| 
;;                                               +---+
;;                                                 x - c->d
;;                                                 |
;;                                               +-+-+ 
;;                                               | d | 
;;                                               +-+-+ 
;;                                                 |
;;                           +                     |
;;                          / \          +------>( < )
;;                         /   \         |
;;                        /     +--------+
;;                       / 0.001 \
;;                      +---------+

;; start
;;   |
;; |guess->b|<------+
;;   |              |
;; |b->c|           |
;;   |              |
;; |c->d|           |
;;   |   yes        |
;; ( < )--->done    |
;;   | no           |
;; |guess->e|       |
;;   |              |
;; |e->guess|-------+

;; (controller
;;    (assign guess (const 1.0))
;;  test-sqrt
;;    (assign b (op square) (reg guess))
;;    (assign c (op -) (reg b) (reg x))
;;    (assign d (op abs) (reg c))
;;    (test (op <) (reg d) (reg x))
;;    (branch (label sqrt-done))
;;    (assign e (op /) (reg guess) (const 0.001))
;;    (assign guess (op average) (reg guess) (reg e))
;;    (goto (label test-sqrt))
;;  sqrt-done)

; Exercise 5.4 ------------------------------------------------------------------

;; a. Recursive exponentiation:

;; Note: There's no need to store the n register as a stack, since it's not used
;; in the calculation.

;; (controller
;;    (assign continue (label expt-done))
;;  expt-loop
;;    (test (op =) (reg n) (const 0))
;;    (branch (label base-case))
;;    (save continue)
;;    (assign n (op -) (reg n) (const 1))
;;    (assign continue (label after-expt))
;;    (goto (label expt-loop))
;;  after-expt
;;    (restore continue)
;;    (assign val (op *) (reg b) (reg val))
;;    (goto (reg continue))
;;  base-case
;;    (assign val (const 1))         
;;    (goto (reg continue))
;;  expt-done)

;; b. Iterative exponentiation:

;; (controller
;;  expt
;;    (assign counter n)
;;    (assign product 1)
;;  expt-iter
;;    (test (op =) (reg counter) (const 0))
;;    (branch (label expt-done))
;;    (assign counter (op -) (reg counter) (const 1))
;;    (assign product (op *) (reg product) (reg b))
;;    (goto (label expt-iter))
;;  expt-done)

; Exercise 5.5 ------------------------------------------------------------------

;; Simulating (fact 3):

(test (op =) 3 1)                    ; false
(assign continue (label fact-done))  ; continue: fact-done
(assign n (op -) 3 1)                ; n: (3 2)
(assign continue (label after-fact)) ; continue: (fact-done after-fact)
(goto (label fact-loop))
(test (op =) 2 1)                    ; false
(assign n (op -) 2 1)                ; n: (3 2 1)
(assign continue (label after-fact)) ; continue: (fact-done after-fact after-fact)
(goto (label fact-loop))
(test (op =) 1 1)                    ; true
(branch (label base-case))
(assign val 1)                       ; val: 1
(goto after-fact)
(restore n)                          ; n: (3 2)
(restore continue)                   ; continue: (fact-done after-fact)
(assign val (op *) 2 1)              ; val: 2
(goto after-fact)
(restore n)                          ; n: (3)
(restore continue)                   ; continue: (fact-done)
(assign val (op *) 3 2)              ; val: 6
(goto fact-done)                     ; Done.

;; Simulating (fib 4):

(assign continue (label fib-done))     ; continue: fib-done
(test (op <) 4 2)                      ; false
(save continue)                        ; cstack: (fib-done)
(assign continue (label afterfib-n-1)) ; continue: afterfib-n-1
(save n)                               ; nstack: (4)
(assign n (op -) 4 1)                  ; n: 3
(goto (label fib-loop))

(test (op <) 3 2)                      ; false
(save continue)                        ; cstack: (fib-done afterfib-n-1)
(assign continue (label afterfib-n-1)) ; continue: afterfib-n-1
(save n)                               ; nstack: (4 3)
(assign n (op -) 3 1)                  ; n: 2
(goto (label fib-loop))

(test (op <) 2 2)                      ; false
(save continue)                        ; cstack: (fib-done afterfib-n-1 afterfib-n-1)
(assign continue (label afterfib-n-1)) ; continue: afterfib-n-1
(save n)                               ; nstack: (4 3 2)
(assign n (op -) 2 1)                  ; n: 1
(goto (label fib-loop))

(test (op <) 1 2)                      ; true
(branch (label immediate-answer))

(assign val 1)                         ; val: 1
(goto afterfib-n-1)

(restore n)                            ; nstack: (4 3), n: 2
(restore continue)                     ; cstack: (fib-done afterfib-n-1), continue: afterfib-n-1
(assign n (op -) 2 2)                  ; n: 0
(save continue)                        ; cstack: (fib-done afterfib-n-1 afterfib-n-1 afterfib-n-1)
(assign continue (label afterfib-n-2)) ; continue: afterfib-n-2
(save val)                             ; vstack: (1)
(goto (label fib-loop))

(test (op <) 0 2)                      ; true
(branch (label immediate-answer))

(assign val 0)                         ; val: 0
(goto afterfib-n-2)
(assign n 0)                           ; n: 0
(restore val)                          ; vstack: (), val: 1
(restore continue)                     ; cstack: (fib-done afterfib-n-1 afterfib-n-1), continue: afterfib-n-1
(assign val (op +) 0 0)                ; val: 0
(goto afterfib-n-1)
(restore n)                            ; nstack: (4), n: 3
(restore continue)                     ; cstack: (fib-done afterfib-n-1), continue: afterfib-n-1
(assign n (op -) 3 2)                  ; n: 1
(save continue)                        ; cstack: (fib-done afterfib-n-1 afterfib-n-1)
(assign continue (label afterfib-n-2)) ; continue: afterfib-n-2
(save val)                             ; vstack: (0)
(goto (label fib-loop))

(test (op <) 1 2)                      ; true
(branch (label immediate-answer))
(assign val 1)                         ; val: 1
(goto afterfib-n-2)
(assign n 1)                           ; n: 1
(restore val)                          ; vstack: (), val: 0
(restore continue)                     ; cstack: (fib-done afterfib-n-1), continue: afterfib-n-2
(assign val (op +) 0 1)                ; val: 1
(goto afterfib-n-2)
(assign n 1)                           ; n: 1
(restore val)                          ; vstack: ?????

;; Ugh, something went wrong. SKIP

; Exercise 5.6 ------------------------------------------------------------------

;; (controller
;;    (assign continue (label fib-done))
;;  fib-loop
;;    (test (op <) (reg n) (const 2))
;;    (branch (label immediate-answer))
;;    (save continue)
;;    (assign continue (label afterfib-n-1))
;;    (save n)
;;    (assign n (op -) (reg n) (const 1))
;;    (goto (label fib-loop))
;;  afterfib-n-1
;;    (restore n)
;;    (restore continue) <--------------------------------- THERE THEY ARE
;;    (assign n (op -) (reg n) (const 2))       |
;;    (save continue) <-------------------------+
;;    (assign continue (label afterfib-n-2))
;;    (save val)
;;    (goto (label fib-loop))
;;  afterfib-n-2
;;    (assign n (reg val))
;;    (restore val)
;;    (restore continue)
;;    (assign val
;;            (op +) (reg val) (reg n)) 
;;    (goto (reg continue))
;;  immediate-answer
;;    (assign val (reg n))
;;    (goto (reg continue))
;;  fib-done)
