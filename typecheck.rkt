#lang racket

(require Desktop/pmatch)
(provide typeof)
(provide consistent?)
(provide meet)



(define consistent?
  (lambda (T1 T2)
    (pmatch `(,T1 ,T2)
            [`(,T1 dyn) #t]
            [`(dyn ,T2) #t]
            [`(int int) #t]
            [`(bool bool) #t]
            [`((-> ,T11 ,T12)(-> ,T21 ,T22)) (and (consistent? T11 T21) (consistent? T12 T22))]
            [`,other #f]
            )))

(define meet 
  (lambda (T1 T2)
    (pmatch `(,T1 ,T2)
            [`(,T1 dyn) T1]
            [`(dyn ,T2) T2]
            [`(int int) 'int]
            [`(bool bool) 'bool]
            [`((-> ,T11 ,T12) (-> ,T21 ,T22)) `(-> ,(meet T11 T21) ,(meet T12 T22))]
            [`,other (error 'meet "types are not consistent")]
            )))


(define typeof
  (lambda (k)
    (pmatch k
            [`,n (guard (integer? n)) 'int]
            [`,b (guard (boolean? b)) 'bool]
            [`inc '(-> int int)]
            [`dec '(-> int int)]
            [`zero? '(-> int bool)]
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
             [`(if ,cnd , thn ,els ,label) 
              (pmatch `(,(typecheck env cnd) ,(typecheck env thn) ,(typecheck env els))
                      [`((,new-cnd ,cnd-T) (,new-thn ,thn-T) (,new-els ,els-T))
                       (cond [(and (consistent? cnd-T 'bool) (consistent? thn-T els-T))
                              (let ([if-T (meet thn-T els-T)])
                                `((if ,(mk-cast label new-cnd cnd-T 'bool) ,(mk-cast label new-thn thn-T if-T) ,(mk-cast label new-thn els-T if-T))))]
                             [else (error 'typecheck "ill-typed if expression")])])]
             [`,x (guard (symbol? x)) `(,x ,(cdr (assq x env)))]
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
                       `((call ,(mk-cast label new-e1 `dyn `(-> ,T2 dyn)) ,new-e2) dyn)]
                      [`((,new-e2 ,T2) (,new-e1 (-> ,T11 ,T12))) 
                         (cond [(consistent? T2 T11) `((call ,new-e1 ,(mk-cast label new-e2 T2 T11)) ,T12)]
                               [else (error 'typecheck "arg/param mismatch")])]
                      [`((,new-e2 ,T2) (new-e1 ,other-T))
                       (error 'typecheck "call to non-function")])]


)))
  

(define-syntax letB
  (syntax-rules ()
    [(letB [x e1] e2)
     (pmatch e1
             [`(blame ,label) `(blame ,label)]
             [`,v (let((v x)) e2)])]))

(define delta
  (lambda (op v)
    (pmatch op
            [`inc(+ 1 v)]
            [`dec(-1 v)]
            [`zero? (= + v)])))

(define shallow-consistent?
  (lambda (T1 T2)
    (pmatch `(,T1 ,T2)
            [`(,T1 dyn) #t]
            [`(dyn ,T2) #t]
            [`(int int) #t]
            [`(bool bool) #t]
            [`((-> ,T11 ,T12) (-> ,T21 ,T22)) #t]
            [`,other #f]
            )
    ))
         



(define apply-cast-id
  (lambda (label1 v T1 T2)
    (cond [(shallow-consistent? T1 T2)
           (pmatch T1
                   [`,dyn (pmatch v
                                 [`(cast ,label2 ,v2 : ,T3 -> dyn)
                                  (apply-cast-id label1 v2 T3 T2)]
                                 )]
                   [`,other (mk-cast label1 v T1 T2)]
                   )]
          [else `(blame ,label1)])))








