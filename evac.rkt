#lang racket

(require Desktop/pmatch)
(require "typecheck.rkt")



(define type-obs (make-hash))


(define (extend-hash hs v T )
  (if (hash-has-key? hs v)
      (hash-set! hs v (append (hash-ref hs v) `(,T)))
      (hash-set! hs v `(,T)))
  
  )




;;; type-obs >> ( list id->type)

;;tipo de entradas
;((lambda (x : dyn) (prim inc (cast l1 x : dyn -> int))) (-> dyn int))






(define (evac env e) 
  (pmatch e
          
          [`,num (guard (integer? num)) num]
          [`,bool (guard (boolean? bool)) bool]
          [`(inc ,e ,label) (guard (integer? e))
                            (let [(e1 (evac env e))]
                              (+ e1 1))]
          [`(dec ,e ,label) (guard (integer? e))
                            (let [(e1 (evac env e))]
                              (- e1 1))]
          
          [`(zero? ,e ,label) (guard (integer? e))
                              (let [(e1 (evac env e))]
                                (zero? e1))]
          
          [`(if ,tst ,ths ,fhs ,label) (let ([tst1 (evac env tst)])
                                         (if (boolean? tst1) 
                                             (if (equal? (typeof (evac env ths)) (typeof (evac env fhs)))
                                                 (if tst1 ths fhs) 
                                                 (error "types are not consistent" label))
                                             (error "test case it is not boolean" label)
                                                 
                                                 ))]
          [`(cast ,label ,e : ,T1 -> ,T2)]
          
          
          [`(lambda (,x : ,T1) ,e ) (extend-hash type-obs x T1 ) `(closure ,x ,T1 ,e ,env)]
          
          
          
          
          
          ))
          
          
;          [`,bool (guard (boolean? bool)) bool]
;          [`,num (guard (number? num)) num]
;          [`(+ ,x ,y) `(+ ,x ,y)]
;          [`(- ,x ,y) `(- ,x ,y)]
;          [`(* ,x ,y) `(* ,x ,y)]
;          [`(/ ,x ,y) `(/ ,x ,y)]
;          [`(> ,x ,y) `(> ,x ,y)]
;          [`(< ,x ,y) `(< ,x ,y)]
;          [`(add1 ,x) (guard (number? x)) (add1 x)]
;          [`,var (guard (symbol? var)) var]
;          [`(if ,tst ,csq ,alt) `(if ,(evac tst env) ,(evac csq env) ,(evac alt env))]
;          ;[`()]
;          [`(sub1 ,x) (sub1 (evac x env))]
;          [`(zero? ,x) `(zero? x) ]
;          [`(lambda (,x) ,body) (local [(define x-id (gensym 'x))] `(closure (,x : ,x-id) ,body ,env))]
;          [`(,rator ,rand) (let ([v1 (evac rator env)]
;                                 [v2 (evac rand env)])
;                             
;                             (pmatch v1
;                                     [`(closure (,x : ,id) ,e ,env2)  
;                                      (set! type-obs `((,id --> ,(typeof v2)) . ,type-obs))                                      
;                                      `((closure (,x : ,(typeof v2)) ,(evac e env2)) --> (typeof return) )
;                                      
;                                      ]
;                                     ))]
;          ))
; 

        
       

          
          
          



