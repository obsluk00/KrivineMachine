#lang scheme
; provide methods for other programm to use it
(provide make-stack)
(provide make-closure)
(provide make-environment)
(provide krivine-state)

; closure data structure: a pair
(define (make-closure)
  (let ((c '()))
    (lambda (msg . args)
      (cond
        ((eq? msg 'set)
          (set! c (cons (car args) (cdr args))))
        ((eq? msg 'fst)
          (car c))
        ((eq? msg 'snd)
          (cadr c))
        ((eq? msg 'get) c)
        (else
          (display (string-append "\nERROR: Not supported command: " (symbol->string msg))) (newline))))))        

; stack data structure with push and pop
(define (make-stack)
  (let ((s '()) (n 0))
     (lambda (msg . args)  ; msg and arguments, if . used, args is seen as list which can be empty
       (cond
         ((eq? msg 'pop)
            (cond
              ((null? s)
                (display "\nERROR: Stack is empty\n"))
              (else
                (set! n (- n 1))
                (define tmpS s)
                (set! s (cdr s))
                (car tmpS)))) ;return the car element of stack
         ((eq? msg 'push)
            (set! n (+ n 1))
            (set! s (append (reverse args) s)))
         ((eq? msg 'get) s)
         ((eq? msg 'size) n)
         ((eq? msg 'display)
            (define clos (make-closure))
            (let loop ((i 0))
               (cond ((< i (length s))
                   (set! clos (list-ref s i))
                   (display "[") (display (clos 'fst)) (display ",") (display (clos 'snd)) (display "] ")
                   (loop (+ i 1))))))
         (else
          (display (string-append "\nERROR: Not supported command: " (symbol->string msg))) (newline))))))
  

#|
; test stack and closures
(define stack (make-stack))
(stack 'pop)   ; display stack is empty
(stack 'invalid)
(define clos (make-closure))
(clos 'set '(ABS (λ . 1) VAR 4 . 4) '())
(stack 'push clos)
(define clos2 (make-closure))
(clos2 'set  '(ABS (λ . 1) VAR 3 . 3) '())
(stack 'push clos2)
(stack 'display) (newline)
(stack 'size)
(stack 'get) 
(define term (stack 'pop)) ; is a closure
(term 'fst)
(set! term (stack 'pop)) ; is a closure
(term 'fst)
|#

; environment data structure: a list
; append arg at the end of list
(define (make-environment)
  (let ((e '()))
    (lambda (msg . args)
      (cond
        ((eq? msg 'append)
          (set! e (append e args)))  ; append takes list, thus no (car args) needed
        ((eq? msg 'getHigh)
          (car e))
        ((eq? msg 'getK)
          (list-ref e (car args))) ; get the kth closure of this environment
        ((eq? msg 'get) e)        
        (else
          (display (string-append "\nERROR: Not supported command: " (symbol->string msg))) (newline))))))


#|
; test env
(define env (make-environment))
(define env2 (make-environment))
(env 'append 1)
(env 'append 2)
(env 'append 3)
(env 'append 4)
(env 'get)
(env2 'append env)
(env2 'append 5)
(env2 'append 6)
(env2 'getK 2)
(env2 'get)
(set! env (env2 'getHigh))
(env 'get)
|#

#|
; test both
(define stack (make-stack))
(define clos (make-closure))
(clos 'set "λx.x" "E")
(stack 'push clos)
(clos 'set "λy.y" "F")
(stack 'push clos)
((stack 'pop) 'snd)
|#

; Krivine Machine state consisting of next to evaluate term T, stack S and environment E
; on object creation elements are initialized
(define (krivine-state)
  (let ((T '()) (S (make-stack)) (E (make-environment)))
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
