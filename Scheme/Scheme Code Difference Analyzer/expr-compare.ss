#lang racket

(define (expr-compare x y)
  (define dictionary (dict-set (hash) '() '()))
  (cond
    [( and (list? x) (list? y) ) (reverse (traverse x y dictionary '() #f))]
    [else (initial x y)]))
    

(define (combine x y)
  (cond
    [ ( or (and ( equal? (car x) 'list) (equal? (car y) 'cons)) (and ( equal? (car x) 'cons) (equal? (car y) 'list))) #t]
    [ ( and ( or ( equal? (car x) 'if) (equal? (car x) 'cons) (equal? (car x) 'list) (equal? (car x) 'quote)) (not(equal? (car x) (car y)))) #f]
    [ ( and ( or ( equal? (car y) 'if) (equal? (car y) 'cons) (equal? (car y) 'list) (equal? (car y) 'quote)) (not(equal? (car x) (car y)))) #f]
    [ ( and ( equal? (car x) 'quote) ( not ( equal? (cdr x) (cdr y)))) #f]
    [ ( and ( equal? (car x) 'list) ( not ( equal? (cdr x) (cdr y)))) #f]
    [ else #t]))

(define (islambda x)
  (cond
    [(or (equal? x 'lambda) (equal? x 'λ)) #t]
    [else #f]))

(define (converge x y)
  (cond
    [(and (equal? x 'lambda) (equal? y 'lambda)) 'lambda]
    [else 'λ]))

(define (create x y dictionary)
  (cond
    [( and (equal? x '()) (equal? y '())) dictionary]
    [else (create (cdr x) (cdr y) (hash-set dictionary (car x) (car y)))]))


(define (initial x y)
  (cond [(equal? x y) x]
    [(and (boolean? x) (boolean? y))(if x '% '(not %))]
    [(or (not (list? x))(not (list? y))) (list 'if '% x y)]))

(define (inside x y dictionary)
  (cond
    [(equal? x y) x]
    [(equal? (hash-ref dictionary x "NO") y) (string->symbol (string-append (symbol->string x) (string-append "!" (symbol->string (hash-ref dictionary x "NO"))))) ]
    [else (list 'if '% x y) ]))

(define (traverse x y dictionary result within)
  (cond
    [( and (equal? x '()) (equal? y '())) result] 
    [( equal? x '()) (cons y result)]
    [( equal? y '()) (cons x result)]
    [(and (not(list? x)) (not (list? y))) (cons (inside x y dictionary) result)]
    [( and (list? (car x) ) (list? (car y)))
     (cond
       [(not( equal? (length x) (length y))) (reverse (list 'if '% x y))]
       [(and (islambda (car (car x))) (islambda (car (car y))))
        ( traverse (cdr x) (cdr y) dictionary (cons ( reverse (cons (reverse(traverse (car(cdr(cdr(car x)))) (car(cdr(cdr (car y)))) (create (car(cdr(car x))) (car(cdr(car y))) dictionary) '() #t)) (cons(reverse (traverse (car(cdr(car x))) (car(cdr(car y))) (create (car(cdr(car x))) (car(cdr(car y))) dictionary)  '() #t)) (list(converge(car(car x)) (car(car y))))))) result) within)]
        [else (traverse (cdr x) (cdr y) dictionary ( cons ( reverse ( traverse (car x) (car y) dictionary '() within)) result) within)])]
    [else
     (cond
       [(equal? within #t) (traverse (cdr x) (cdr y) dictionary (cons (inside (car x) (car y) dictionary) result) within)]
       [(combine x y) (traverse (cdr x) (cdr y) dictionary (cons (initial (car x) (car y)) result) within)]
       [else (reverse (list 'if '% x y))])]))
    

;2
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '([%#t]) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '([%#f]) (expr-compare x y))))))
;3
(define test-expr-x '(+ 3 ((lambda (a b) (list a b)) 1 2)))
(define test-expr-y '(+ 2 ((lambda (a c) (list a c)) 1 2)))