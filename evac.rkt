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
            [`,other ( error "variable not find")]
            )))

(define (extend-hash hs v T )
  (unless (not (pair? T))
    (extend-hash hs v (car T))
    (extend-hash hs v (cdr T))
    )
  
  (unless (pair? T)
    (unless (empty? T)
    (if (hash-has-key? hs v)
        
       (unless (memq T (hash-ref hs v))
          
    (hash-set! hs v (append (hash-ref hs v) (list T))))
            
    (hash-set! hs v `(,T))))))

(define (type-lookup hs v T)
  (if (hash-has-key? hs v)
      (meet2 (hash-ref hs v))
      T))

(define (meet2 lst)
  (cond [(and (memq 'int lst) (memq 'bool lst)) 'dyn ]
        [(memq 'int lst) 'int]
        [(memq 'bool lst) 'bool]
        [else 'dyn]
        )
  )





(define (evac env e) 
  (pmatch e
          
          [`,num (guard (integer? num)) num]
          [`,bool (guard (boolean? bool)) bool]
          [`(inc ,e) (guard (integer? e))
                            (let [(e1 (evac env e))]
                              (+ e1 1))]
          
         
          [`(dec ,e) (guard (integer? e))
                            (let [(e1 (evac env e))]
                              (- e1 1))]
          
          [`(zero? ,e) (guard (integer? e))
                              (let [(e1 (evac env e))]
                                (zero? e1))]
          
          
          [`(cast ,label ,e : ,T1 -> ,T2) (extend-hash type-obs e T1)
                                          (extend-hash type-obs e T2)
                                          (evac env e)]
          
          [`(prim ,op ,e) (if (member op '(inc zero? dec))
                              (let ([v1 (evac env e)])
                                (if (integer? v1) (evac env `(,op ,(evac env e)))
                                (error "expretion invalid"))
                                  )
                              (error "operator does not exist"))]
          
          [`(if ,tst ,ths ,fhs ,label) (let ([tst1 (evac env tst)])
                                         (if (boolean? tst1) 
                                             (if (equal? (typeof (evac env ths)) (typeof (evac env fhs)))
                                                 (if tst1 ths fhs) 
                                                 (error "types are not consistent" label))
                                             (error "test case it is not boolean" label)
                                                 
                                                 ))]

          
          [`(lambda (,x : ,T1) ,e ) (extend-hash type-obs x T1 ) 
                                    `(lambda (,x : ,T1) ,e ,env)]
          
       
          
          [`(,e1 ,e2) 
                      (let ([v1 (evac env e1)]
                            [v2 (evac env e2)])
                      (pmatch v1
                              [`(lambda (,x : ,T1) ,e ,env2) 
                               (extend-hash type-obs e T1)
                               (extend-hash type-obs e (typeof v2))
                               (evac (extend-env env2 x v2) e)
                                                         
                                                         ]
                              [`,other (error "e1,e2")]
                              ))]
          [`,var (env-lookup env var)]
          
          )
          )
          

(define final-type
  (lambda (env exp)
   
    (pmatch exp
            
          [`,num (guard (integer? num)) num]
          [`,bool (guard (boolean? bool)) bool]
            
          [`(cast ,label ,e : ,T1 -> ,T2) 

                                           (final-type env e)]
            
          [`(prim ,op ,e) (if (member op '(inc zero? dec))
                                `(,op ,(final-type env e))
                                (error "operator does not exist"))]

            
          [`(lambda (,x : ,T1) ,e ) `(lambda (,x : ,(type-lookup type-obs x T1)) ,e )]
            
            [`(,e1 ,e2)
                      (let ([v1 (final-type env e1)]
                            [v2 (final-type env e2)])
                        `(,v1 ,v2))
                      
            ]
            [`,var var]
            
            
            
            
            )))




(define (run exp)
  (hash-clear! type-obs)
  (let [(x (typecheck '() exp))]
   (displayln `(typed ,x))
   (displayln `("evaluation-->" ,(evac '() (car x))))

         (final-type '() (car x))
    ;     )
    ))
    

(define test1 '((lambda (x1) (inc x1 l1))10 l2))
;(displayln `(test1 ,test1))
(run test1) 


(define test2 '((lambda (x2 : dyn) (x2 42 l2)) (lambda (y2 : dyn) y2) l1))
;(displayln `(test2 ,test2))
(run test2)


(define test10 '((lambda (x2 : dyn) ((lambda (y2 : int)y2) x2 l1))5 l2))
;(displayln `(test10 ,test10))
(run test10)


(define test11 '((lambda (x : dyn) ((lambda (y : dyn) (x #t l1)) (x 42 l2) l3)) (lambda (z : dyn) z) l4))
;(displayln `(test11 ,test11))
(run test11)

(define test12 '((lambda (x4 : dyn) (x4 10 l1)) (lambda (z4 : dyn) z4) l2))
(run test12)



