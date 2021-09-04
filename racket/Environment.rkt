#lang scheme
; provide method for other routine to use it
(provide make-environment)

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
