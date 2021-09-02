#lang scheme

; need to call disp-list from other .rkt
(provide disp-list)

; Method to display each list element on new line
(define (disp-list lst)
  (cond ((null? lst)
            '())
        (else
            (display (car lst)) (newline)
            (disp-list (cdr lst)))))

; put the string to a list
(define l (string->list "((λy.x)((λx.x)(λz.z)))((λv.vw)x)"))

; Method to list of chars @ '(' and ')' to create list of lists
; takes as arguments the list to split and a current result list
; the latter is empty on first call
(define nP 0)  ; control for matching parantheses
(define (tokenize lst curr)
  (cond ((null? lst)
            (if (eq? nP 0)
                (cons curr '()) ; break condition
                (display "\nERROR: Non mathing parentheses in Input\n")))
        ((eq? #\( (car lst))
            (set! nP (+ nP 1)) 
            (let ((rslt (tokenize (cdr lst) (list (car lst)))))
              (if (null? curr)
                  rslt
                  (cons curr rslt))))
        ((eq? #\) (car lst))
            (set! nP (- nP 1))
            (let ((last (list-ref curr (- (length curr) 1)))) ; last element of curr
              (cond ((not (eq? #\) last))  ; if last element is not ")" append
                        (tokenize (cdr lst) (append curr (list (car lst)))))
                    (else
                        ; else start new as its an outer parentheses
                        (let ((rslt (tokenize (cdr lst) (list (car lst)))))
                           (cons curr rslt))))))                 
        (else
         ; jsut appand ordinary symbol
            (tokenize (cdr lst) (append curr (list (car lst)))))))

; call tokenize on l with current set to empty list '() and save it in new object
(define token (tokenize l '()))

; display l
;(disp-list token)
   
;(cadr token)
;(display (cadr token))



