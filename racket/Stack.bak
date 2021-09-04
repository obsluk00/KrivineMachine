#lang scheme
; provide method for other routine to use it
(provide make-stack)

; require Closure
(require "Closure.rkt")

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