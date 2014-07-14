#lang racket

(require Desktop/pmatch)
(require "typecheck.rkt")



(define type-obs (make-hash))

(define extend-env
  (lambda (env fml rand)
    `((,fml ,rand) . ,env)))

(define env-lookup 
  (lambda(env var)
    (pmatch env    
            [`((,x ,v) . ,rest) (if (eq? x var) v (env-lookup (cdr env) var))]
            )))

(define (extend-hash hs v T )
  (if (hash-has-key? hs v)
      (hash-set! hs v (meet T (hash-ref hs v)))
      (hash-set! hs v T))
  
  )


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
          
          [`(cast ,label ,e : ,T1 -> ,T2) (extend-hash type-obs e (meet T1 T2)) `,e]
          
          [`(prim ,op ,e) (if (member op '(inc zero? dec)) 
                              `(,op ,(evac env e))
                              (error "operator does not exist"))]
          
          [`(if ,tst ,ths ,fhs ,label) (let ([tst1 (evac env tst)])
                                         (if (boolean? tst1) 
                                             (if (equal? (typeof (evac env ths)) (typeof (evac env fhs)))
                                                 (if tst1 ths fhs) 
                                                 (error "types are not consistent" label))
                                             (error "test case it is not boolean" label)
                                                 
                                                 ))]
          
          
          [`(lambda (,x : ,T1) ,e ) (extend-hash type-obs x T1 ) `(closure ,x ,T1 ,e ,env)]
          
          [`(,e1 ,e2) (let ([v1 (evac env e1)]
                            [v2 (evac env e2)])
                        ; (display `(v1 ,v1 v2 ,v2))
                      (pmatch v1
                              [`(closure ,x ,T1 ,e ,env) (extend-hash type-obs x (meet T1 (typeof v2)))
                                                         (evac (extend-env env x v2) e)
                                                         
                                                         ]))]
          
          

           
                               
                     
          )
          )
          


          
   
       

          
          
          



