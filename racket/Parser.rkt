#lang scheme

(provide compile)
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

(define (make-variable v k)
  (attach-tag 'VAR (cons v k)))

;helper to find index of character in string
(define (get-pos-of string char)
  (let loop ((list (string->list string))
             (index 0))
    (cond ((empty? list) #f)
          ((equal? (car list) char) index)
          (else (loop (cdr list) (+ 1 index))))))

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


;repl
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

;creates ast
(define (parse input)
  (cond ((and (list? input) (empty? (cdr input)))
         (parse (car input)))
        ((and (list? input) (not (empty? input)))
         (make-application (parse (car input)) (parse (cdr input))))
        ((symbol? input)
         (let ([stringput (symbol->string input)])
           (cond ((get-pos-of stringput #\λ)
                  (let ([splitted (split-abstraction stringput)])
                    (make-abstraction (car splitted) (parse (cdr splitted)))))
                 (else
                  (attach-tag 'VAR stringput)))))))

;bindings are tracked as a list where every lambda prepends a list of the variables it bounded (in order of binding)
;TODO: returns #f if var isnt bound, otherwise it returns a pair of depth when bound (used to calculate v) and the how many-th argument the variable is (k)
(define (bound? var bound-list)
  (cond ((empty? bound-list)
         #f)
        ((index-of (caar bound-list) var)
         (cons (cdr (car bound-list)) (index-of (caar bound-list) var)))
        (else (bound? var (cdr bound-list)))))

;adds variable/s being bound at depth to the bound-list
(define (bind binding depth bound-list)
  (cons (cons (binding-helper binding) depth) bound-list))

;creates list of variables (as symbols) that are bound by the first part of an abstraction
(define (binding-helper binding)
  (let ([listed (for/list ([chars (string->list (symbol->string binding))])
                  (string->symbol (string chars)))])
    (cdr listed)))

;"compiles" first part of an abstraction
(define (lambda-computer term)
  (let ([stringput (symbol->string (car term))])
    (cons #\λ (- (string-length stringput) 1))))

(define (compile-abstraction term depth bound-list)
  (let ([new-bound-list (bind (car term) depth bound-list)])
  (make-abstraction (lambda-computer term) (compile (cdr term) (+ 1 depth) new-bound-list))))

(define (compile-variable var depth bound-list)
  (let ([binding-info (bound? var bound-list)])
    (cond (binding-info
           (make-variable (- depth (car binding-info)) (cdr binding-info)))
          ((not binding-info)
           (make-variable (+ depth (hash-ref encode-var (symbol->string var))) 'INF)))))

(define (compile term depth bound-list)
  (cond ((eq? 'APP (car term))
         (make-application (compile (car (cdr term)) depth bound-list) (compile (cdr (cdr term)) depth bound-list)))
        ((eq? 'ABS (car term))
         (compile-abstraction (cdr term) depth bound-list))
        ((eq? 'VAR (car term))
         (compile-variable (cdr term) depth bound-list))))



