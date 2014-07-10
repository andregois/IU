#lang racket

(require Desktop/pmatch)
(require rackunit)

; listof : (A -> B) (listof A) -> (listof B)
(define (mymap f l)
  (cond 
    [(null? l) '()]
    [(pair? (car l)) (cons (mymap f (car l)) (mymap f (cdr l)))]
    [else (cons (f (car l)) (mymap f (cdr l)))]
  ))



(check-equal? (mymap add1 empty) empty)
(check-equal? (mymap add1 (list 1 2 3)) (list 2 3 4))
(check-equal? (mymap even? (list 1 2 3)) (list #f #t #f))


(define (reverse-symbol x)
  (cond
    [(symbol? x)(string->symbol (list->string (reverse (string->list (symbol->string x)))))]
    [else x]
  ))

(define env-lookup 
  (lambda(env var)
    (pmatch env    
            [`((,x ,v) . ,rest) (if (eq? x var) v (env-lookup (cdr env) var))]
            )))

(define closure
  (lambda(fml env code)
    
    `(closure ,fml ,env ,code)))

(define closure-apply
  (lambda (rator rand)
    (pmatch rator
            [`(closure ,fml ,env ,code) (eval-1 code (env-extend env fml rand))]
            )))

(define env-extend 
  (lambda (env fml rand)
    `((,fml ,rand) . ,env)))

 
(define eval-1
  (lambda (exp env)
    (pmatch exp 
            [`(lambda (,x) ,body) (closure x env body)]
            [`,bool (guard (boolean? bool)) bool]
            [`,num (guard (number? num)) num]
            [`(+ ,x ,y) (+ (eval-1 x env) (eval-1 y env))]
            [`(- ,x ,y) (- (eval-1 x env) (eval-1 y env))]
            [`(* ,x ,y) (* (eval-1 x env) (eval-1 y env))]
            [`(/ ,x ,y) (/ (eval-1 x env) (eval-1 y env))]
            [`(add1 ,x) (guard (number? x)) (add1 x)]
            [`,var (guard (symbol? var)) (env-lookup env var)]
            [`(if ,tst ,csq ,alt) (if (eval-1 tst env) (eval-1 csq env) (eval-1 alt env))]
            [`(sub1 ,x) (sub1 (eval-1 x env))]
            [`(zero? ,x) (zero? (eval-1 x env)) ]
            [`(,operator ,operand) (closure-apply (eval-1 operator env) (eval-1 operand env))]
            [`,var (guard (symbol? var)) (unbox (env-lookup env var))]
            [`(set! ,var ,exp) (set-box! (env-lookup env var) (eval-1 exp env))]
            )))

;;----------------------------------------------------------------------------------------------------


;;-----------------------------------------------------------------------------------------------------
(define value-of-ds
  (lambda (exp)
    (eval-1 exp '())))

(define (empty-env) '())

(define fo-eulav
  (lambda (exp env)
    (eval-1 (deep-reverse exp) env)
      ))


(define deep-reverse
  (lambda (x)
    (cond
      ((symbol? x) (reverse-symbol x))
      ((pair? x) (map deep-reverse (list-reverse x)))
      (else x))))

(define (list-reverse ls)
  (pmatch ls
   [`() `()]
   [`(,a . ,d) (append (list-reverse d) (list a))]))



(define (type-return x)
  (pmatch x 
          [`(,func) ((function-return func))]
          [`,var ((type-checker var))]

          ))

(define (type-checker x)
  (cond
    [(boolean? x) 'boolean]
    [(number? x) 'number]
    [(symbol? x) 'symbol]
    [(string? x) 'string]
    ))

(define (function-return func)
  (cond
    [(boolean? func) 'boolean]
    [(number? func) 'number]
    [(symbol? func) 'symbol]
    [(string? func) 'string]
    ))
   

