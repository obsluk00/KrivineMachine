#lang scheme
; require Data Structures and Loop Methods defined in DataStructures.rkt
(require "DataStructures.rkt")
; require Parser
(require "Parser.rkt")

(define input  '(((λy.y)((λx.x)(λz.z)))(λv.v)))
;(define input  '((λx.x x)(λx.x)))
(define parsed '(APP (APP (ABS (#\λ . 1) VAR . (  0 . 1)) APP (ABS (#\λ . 1) VAR . ( 0 . 1)) ABS (#\λ . 1) VAR . ( 0 . 1)) ABS (#\λ . 1) VAR . ( 4 . 4)))


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
     (cond ((> i 0)
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
  (define e (make-environment))
  (let ((v (cadr (s 'getT))) (k (cddr (s 'getT))))
    ;(display (string-append "v:" (number->string v) " k:" (number->string k))) (newline)
    ; get recursive the environment v and update state
    (let loop ((i 0))
     (cond ((< i v)          
         (set! e (env 'getHigh))
         (set! env e)
         (cond ((null? env)
             (error "Empty environment found. Machine stopped" (env 'get))))         
         (loop (+ i 1)))))
    ; get k closure in the updated state environment and update T, check if this is available
    (cond ((> k (length(env 'get)))
               (error "No Closure with needed index in environment. Machine stopped" (env 'get)))
          (else
               (let ((T '()) (E '()) (clos (make-closure)))
                   (set! clos (env 'getK k))
                   (set! T (clos 'fst))
                   (set! E (clos 'snd))
                   (s 'setT T) ; elements of clos are T and E
                   (s 'setE E))
               s)))) ; return new state


; Krivine Machine
(define (krivine-machine T)
  ; create state object
  (define state (krivine-state))
  (state 'setT T)
  (define n 0)
  ; eval loop
  (let eval ((state state) (n n))
     (display (string-append "State after step " (number->string n) ":")) (newline) 
     (display "T: ") (display (state 'getT)) (newline)
     (display "S: ") ((state 'getS) 'display) (newline)
     (display "E: ") (display ((state 'getE) 'get)) (newline) (newline)
     (cond ((eq? (car (state 'getT)) 'APP)
            (eval (app state) (+ n 1)))
           ((and (not (null? ((state 'getS) 'get))) (eq? (car (state 'getT)) 'ABS))
            ; start abstraction only if stack is non empty
            (eval (abs state) (+ n 1)))
           ((eq? (car (state 'getT)) 'VAR)
            (eval (var state) (+ n 1)))
           ((and (null? ((state 'getS) 'get)) (null? ((state 'getE) 'get)))
            state)  ; termination condition
           ))
  (state 'getT))

(krivine-machine (compile (parse input) 0 '()))
;(krivine-machine parsed)
