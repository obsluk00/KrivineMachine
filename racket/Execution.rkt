#lang scheme
; require Data Structures and Loop Methods defined in DataStructures.rkt
(require "DataStructures.rkt")

; parser.rkt neede to test
(require "Parser.rkt")
(define input  '(((λy.x)((λx.x)(λz.z)))(λv.vw)))

; we have:
;    T: Term to execute. From parsing we get pairs
;    S: The stack. At first empty
;    E: An environment. At first its the empty Environment
;    The three are contained in the krivine-state Object, handled with 'get and 'set

; Execution has 3 rules
; 1. Application of the form (λx.x)(λy.y): APP (ABS (#\λ . 1) VAR . "x") ABS (#\λ . 1) VAR . "z")
;    Create closure with the address of the latter part (e.g. λy.y), push to stack and update T
;    @Param: a state object
;    @Return: a state object

(define (app s) 
  (define clos (make-closure))
  (clos 'set (cddr (s 'getT)) (s 'getE)) ; create closure with second part of T pair as new T and the current environment
  (define stack (s 'getS))
  (stack 'push clos)
  (s 'setS stack)  ; push it to stack
  (s 'setT (cadr (s 'getT))) ; T is the first element of the pair which is evaluated next
  s) ; resturn new state

; 2. Abstraction λx.x: (ABS (#\λ . 1) VAR . "u")
;    Create new environment e.g (E, clos1, ..., closN), by popping n times from stack
;    @Param: a state object
;    @Return: a state object
(define (abs s)
  (define env (make-environment))
  (env 'append (s 'getE)) ; update E, the pointer to current environment
  (define stack (s 'getS))
  (let loop ((i (cdr (cadr (s 'getT))))) ; do number of pops
     (cond ((or (> i 0) (= i 0))
         (env 'append (stack 'pop))
         (loop (- i 1)))))
  (s 'setE env) ; this is new state environment
  (s 'setS stack) ; new stack
  (s 'setT (cddr (s 'getT))) ;T becomes the last element of former T pair
   s) ; return new state

; 3. Variable x
;    The Value of x was replaced by an ordered pair <v,k>
;    The v gives the environment in which x can be found
;    k is the number of the closure inside that environment
;    @Param: a state object
;    @Return: a state object
(define (var s)
  (define env (s 'getE))
  (let ((v (cadr (s 'getT))) (k (cddr (s 'getT))))
    ; get recursive the environment v and update state
    (let loop ((i 0))
     (cond ((or (< i v) (= i v))
         (set! env (env 'getHigh))
         (loop (+ i 1)))))
    (s 'setE env)
    ; get k closure in the updated state environment and update T
    (s 'setT (list-ref (s 'getE) k)))
     s) ; return new state


; Krivine Machine
(define (krivine-machine T)
  ; create all state object
  (define state (krivine-state))
  (state 'setT T)
  ; eval loop
  (let eval ((state state))
     (cond ((eq? (car (state 'getT)) 'APP)
            (eval (app state)))
           ((eq? (car (state 'getT)) 'ABS)
            (eval (abs state)))
           ((eq? (car (state 'getT)) 'VAR)
            (eval (var state)))
           ((and (null? (state 'getS)) (null? (state 'getE)))
            (state))))
  (state 'getT))

(krivine-machine (parse input))
