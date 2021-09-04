#lang scheme
; provide method for other routine to use it
(provide make-closure)

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