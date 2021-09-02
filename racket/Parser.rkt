#lang scheme

(provide parse)

;typesystem for lambda calculus
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag typed-content)
  (car typed-content))

(define (content typed-content)
  (cdr typed-content))

(define (make-application f x)
  (attach-tag 'APP (cons f x)))

(define (make-abstraction binding body)
  (attach-tag 'ABS (cons binding body)))

(define (make-variable x)
  (attach-tag 'VAR x))

;compiles terms for evaluation

;helper to find index of character in string
(define (get-pos-of string char)
  (let loop ((list (string->list string))
             (index 0))
    (cond ((empty? list) #f)
          ((equal? (car list) char) index)
          (else (loop (cdr list) (+ 1 index))))))

;"compiles" first part of an abstraction
(define (lambda-computer term)
  (cons #\λ (- (string-length (symbol->string term)) 1)))
    
;"compiles" variables by computing the <v,k> pair
;free variables are assigned a numerical encoding according to a->1, b->2, ...
(define encode-var #hash(("a" . 1)
                         ("b" . 2)
                         ("c" . 3)
                         ("d" . 4)
                         ("e" . 5)
                         ("f" . 6)
                         ("g" . 7)
                         ("h" . 8)
                         ("i" . 9)
                         ("j" . 10)
                         ("k" . 11)
                         ("l" . 12)
                         ("m" . 13)
                         ("n" . 14)
                         ("o" . 15)
                         ("p" . 16)
                         ("q" . 17)
                         ("r" . 18)
                         ("s" . 19)
                         ("t" . 20)
                         ("u" . 21)
                         ("v" . 22)
                         ("w" . 23)
                         ("x" . 24)
                         ("y" . 25)
                         ("z" . 26)))

(define decode-var #hash((1 . "a")
                         (2 . "b")
                         (3 . "c")
                         (4 . "d")
                         (5 . "e")
                         (6 . "f")
                         (7 . "g")
                         (8 . "h")
                         (9 . "i")
                         (10 . "j")
                         (11 . "k")
                         (12 . "l")
                         (13 . "m")
                         (14 . "n")
                         (15 . "o")
                         (16 . "p")
                         (17 . "q")
                         (18 . "r")
                         (19 . "s")
                         (20 . "t")
                         (21 . "u")
                         (22 . "v")
                         (23 . "w")
                         (24 . "x")
                         (25 . "y")
                         (26 . "z")))

(define (compile-variable term var)
  (cond [(eq? term (string var))
         (cons 0 'inf)]
        ))

;repl
;TODO: padding of input with brackets
(define (repl)
  (begin
    (display "Input: ")
    (let ([input (read)])
      (newline)
      (display (parse input)))
    (newline)
    (repl)))


;creates pair of bindings and body for a given abstraction
(define (split-abstraction term)
  (let ([dot-index (get-pos-of term #\.)])
    (cons (string->symbol (substring term 0 dot-index)) (string->symbol (substring term (+ 1 dot-index))))))

(define (parse input)
  (cond ((and (list? input) (empty? (cdr input)))
         (parse (car input)))
        ((and (list? input) (not (empty? input)))
         (make-application (parse (car input)) (parse (cdr input))))
        ((symbol? input)
         (let ([stringput (symbol->string input)])
           (cond ((get-pos-of stringput #\λ)
                  (make-abstraction (lambda-computer (car (split-abstraction stringput))) (parse (cdr (split-abstraction stringput)))))
                 (else
                  (make-variable stringput)))))))






