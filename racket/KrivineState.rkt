#lang scheme
; provide method for other routine to use it
(provide krivine-state)

; require Stack and Environment
(require "Stack.rkt")
(require "Environment.rkt")

; Krivine Machine state consisting of next to evaluate term T, stack S and environment E
(define (krivine-state)
  (let ((T '()) (S (make-stack)) (E (make-environment)))  ; on object creation elements are initialized
    (lambda (msg . args)  ; args is always a list, to get sinlge element use (car args)
       (cond
          ((eq? msg 'setT) (set! T (car args)))
          ((eq? msg 'setS) (set! S (car args))) 
          ((eq? msg 'setE) (set! E (car args))) 
          ((eq? msg 'getT) T)
          ((eq? msg 'getS) S)
          ((eq? msg 'getE) E)
          (else
             (display (string-append "\nERROR: Not supported command: " (symbol->string msg))) (newline))))))

#|
; test state
(define state (krivine-state))
(state 'getS)
(define stack (state 'getS))
(stack 'push "λx.x")
(stack 'push "λy.y")
(state 'setS stack)
(state 'getS)
((state 'getS) 'pop)
((state 'getS) 'pop)

; this does not work in one line 
(define state2 (krivine-state))
(state 'getS)
(state2 'setS ((state2 'getS) 'push "λx.x"))
(state2 'getS)  ; after its not a stack object anymore, no clue why??
|#
