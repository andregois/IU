#lang racket

(require Desktop/pmatch)
(provide typeof)
(provide consistent?)
(provide meet)
(provide typecheck)



(define consistent?
  (lambda (T1 T2)
    (pmatch `(,T1 ,T2)
            [`(,T1 dyn) #t]
            [`(dyn ,T2) #t]
            [`(int ,T2) #t]
            [`(,T1 int) #t]
            [`(bool ,T2) #t]
            [`(,T1 bool) #t]            
            [`((-> ,T11 ,T12)(-> ,T21 ,T22)) (and (consistent? T11 T21) (consistent? T12 T22))]
            [`,other #f]
            )))

(define meet 
  (lambda (T1 T2)
    (pmatch `(,T1 ,T2)
            [`(int int) 'int]
            [`(bool bool) 'bool]
            [`(int ,T2) 'dyn]
            [`(,T1 int) 'dyn]
            [`(bool ,T2) 'dyn]
            [`(,T1 bool) 'dyn]
            [`(,T1 dyn) 'dyn]
            [`(dyn ,T2) 'dyn]            
            [`((-> ,T11 ,T12) (-> ,T21 ,T22)) `(-> ,(meet T11 T21) ,(meet T12 T22))]
            [`,other (error T1 T2)]
            )))


(define typeof
  (lambda (k)
    (pmatch k
            [`,n (guard (integer? n)) 'int]
            [`,b (guard (boolean? b)) 'bool]
            [`inc '(-> int int)]
            [`dec '(-> int int)]
            [`zero? '(-> int bool)]
            [`,other 'dyn]
            )))

(define (prim op mk)
  #t)

(define (call mk1 mk2)
  #t)


(define mk-cast 
  (lambda (label e T1 T2)
          (cond [(equal? T1 T2) e]
                [else `(cast ,label ,e : ,T1 -> ,T2)]
                )))



(define-syntax letT
  (syntax-rules() [(letT [x : T label = e] b)
  `((lambda(x : T),b)e label)]))



(define eg1
  (letT [f0 : dyn 0 = (lambda (x : int) (inc x 2))]
        (letT [f1 : (-> bool bool) 1 = f0]
        '(f1 #t 3))))



(define constant? 
  (lambda  (k) (or (integer? k) (boolean? k))))


(define operator? 
  (lambda (op) (memq op '(inc dec zero?))))




(define typecheck
  (lambda (env e)
    (pmatch e
             [`,k (guard(constant? k)) `(,k ,(typeof k))]
             [`(,op ,e ,label) (guard (operator? op))
                               (pmatch `(,(typecheck env e) ,(typeof op))
                                        [`((,new-e ,T) (-> ,T1 ,T2)) (guard (consistent? T T1)) `((prim ,op ,(mk-cast label new-e T T1)) ,T2)]
                                        [`,other (error 'typecheck "primitive operator")])]
             [`(if ,cnd ,thn ,els ,label) 
              (pmatch `(,(typecheck env cnd) ,(typecheck env thn) ,(typecheck env els))
                      [`((,new-cnd ,cnd-T) (,new-thn ,thn-T) (,new-els ,els-T))
                       (cond [(and (consistent? cnd-T 'bool) (consistent? thn-T els-T))
                              (let ([if-T (meet thn-T els-T)])
                                `((if ,(mk-cast label new-cnd cnd-T 'bool) ,(mk-cast label new-thn thn-T if-T) ,(mk-cast label new-els els-T if-T))))]
                             ; era (mk-cast label new-ths els-T if-T))))]
                             [else (error 'typecheck "ill-typed if expression")])])]
             [`,x (guard (symbol? x)) `(,x ,(cdr (assq x env)))]
             [`((lambda (,x : ,T1) ,e),T) (typecheck env `(lambda(,x : ,T1) ,e))]
             [`(lambda (,x) ,e) (typecheck env `(lambda(,x : dyn) ,e))]
             [`(lambda (,x : ,T1) ,e)
              (pmatch `,(typecheck `((,x . ,T1) . ,env) e)
                      [`(,new-e ,ret-T) `((lambda (,x : ,T1) ,new-e)(-> ,T1 ,ret-T))]
                      )]
             [`(,e : ,T ,label) 
              (pmatch `,(typecheck env e) 
                      [`(,new-e ,e-T)
                       (cond [(consistent? e-T T) `(,(mk-cast label new-e e-T T) ,T)]
                             [else (error 'typecheck "cast between inconsistent types")])]
                      )]
             [`(,e1 ,e2 ,label)
              (pmatch `(,(typecheck env e2) ,(typecheck env e1))
                      [`((,new-e2 ,T2) (,new-e1 dyn))
                       `((,(mk-cast label new-e1 `dyn `(-> ,T2 dyn)) ,new-e2) dyn)]
                      [`((,new-e2 ,T2) (,new-e1 (-> ,T11 ,T12)))
                         (cond [(consistent? T2 T11) `((,new-e1 ,(mk-cast label new-e2 T2 T11)) ,T12)]
                               [else (error 'typecheck "arg/param mismatch")])]
                      [`((,new-e2 ,T2) (new-e1 ,other-T))
                       (error 'typecheck "call to non-function")])]


)))
  







