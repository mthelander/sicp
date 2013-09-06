; Exercise 1.1

; Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b))) ; 4
    b
    a)
(cond ((= a 4) 6) ; 16
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a) ; 16
         ((< a b) b)
         (else -1))
   (+ a 1))

; Exercise 1.2

; Translate the following expression into prefix form

; (5+1+(2-(3-(6+1/5))))/(3(6-2)(2-7))

(/ (+ 5 1 (- 2 (- 3 (+ 6 (/ 1 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3

; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))
(define (sum-of-squares-of-two-largest x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(sum-of-squares-of-two-largest 5 6 7) ; 85
(sum-of-squares-of-two-largest 3 1 9) ; 90
(sum-of-squares-of-two-largest 5 4 3) ; 41

; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:
;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))

; The first expression evaluated is a compound expression; it's an if statement which determines the operator that will operate on the rest of the operands. If b is greater than 0 then it sums the parameters, othersise it subtracts b from a.

; Exercise 1.5

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

; If the interpreter uses normal-order evaluation then the expression will
; evaluate to 0, because it will expand the procedure before the arguments
; are evaluated, and because the first parameter is 0, the if statement will
; return before the second is ever evaluated. If it were evaluated, then it
; would cause an out of memory error because of an infitely recursive procedure.

; Exercise 1.6

; Bad things will happen because normal procedure calls use applicative-order
; evaluation, so both the then-clause and the else-clause will be evaluated when
; she uses the new-if procedure. This will cause the sqrt procedure to infinitely
; recurse.

; Exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt .0003) ; .03438350032699598 -- should be .0173205
