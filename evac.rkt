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
    (if (hash-has-key? hs v)
        (hash-set! hs v (meet T (hash-ref hs v)))
        (hash-set! hs v T)))

(define (type-lookup hs v T)
  (if (hash-has-key? hs v)
      (hash-ref hs v)
      T))


;;tipo de entradas
;((lambda (x : dyn) (prim inc (cast l1 x : dyn -> int))) (-> dyn int))
;((lambda (x : dyn) (prim inc (cast l1 x : dyn -> int))) (-> dyn int))8))
;((lambda (x : dyn) x) (-> dyn dyn))
;((lambda (x : dyn) (prim zero? (cast l2 x : dyn -> int))) (-> dyn bool))




(define (evac env e) 
  (pmatch e
          
          [`,num (guard (integer? num)) num]
          [`,bool (guard (boolean? bool)) bool]
          [`(inc ,e) (guard (integer? (env-lookup env e)))
                            (let [(e1 (evac env (env-lookup env e)))]
                              (+ e1 1))]
          
;((call ,new-e1 ,(mk-cast label new-e2 T2 T11)) ,T12)
;((call ,(mk-cast label new-e1 `dyn `(-> ,T2 dyn)) ,new-e2) dyn)]

          [`(call ,p1 ,p2) (let ([v1 (evac env p1)]
                                      [v2 (evac env p2)])
                                             (display `(v1 ,v1 ,p1 v2 ,v2))                
                                  (pmatch v1
                                          [`(lambda (,x : ,T) ,e ,env2) (extend-hash type-obs x (meet T (typeof v2))) 
                                                                        (evac (extend-env env2 x v2) e)] 
                                          
                                          )
                                                                                                      
                                 )]
          
          [`(dec ,e) (guard (integer? (env-lookup env e)))
                            (let [(e1 (evac env (env-lookup env e)))]
                              (- e1 1))]
          
          [`(zero? ,e) (guard (integer? (env-lookup env e)))
                              (let [(e1 (evac env (env-lookup env e)))]
                                (zero? e1))]
          
          
          [`(cast ,label ,e : ,T1 -> ,T2) (extend-hash type-obs e (meet T1 T2)) 
                                          (evac env e)]
          
          [`(prim ,op ,e) (if (member op '(inc zero? dec)) 
                              (if (integer? (env-lookup env e)) (evac env `(,op ,(evac env e))) 
                                  `(,op ,(evac env e))
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
          
          [`(,e1 ,e2) ;(display `(e1 ,e1 e2 ,e2)) 
                      (let ([v1 (evac env e1)]
                            [v2 (evac env e2)])
                      ; (display `(v1 ,v1 v2 ,v2))
                      (pmatch v1
                              [`(lambda (,x : ,T1) ,e ,env2) ;(display 'aqui)
                                                        (extend-hash type-obs x (meet T1 (typeof v2)))
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
            [`(cast ,label ,e : ,T1 -> ,T2) e]
            [`(prim ,op ,e) (if (member op '(inc zero? dec))
                                `(,op ,(final-type env e) ,(gensym))
                                (error "operator does not exist"))]
            [`((lambda (,x : ,T1) ,e ) ,T) (pmatch T
                                                 [`(-> ,T2 ,T3) `((lambda (,x : ,(type-lookup type-obs x T1)) ,(final-type env e)),T)]
                                                 )]
            [`(lambda (,x : ,T) ,e ) ;(display 'd) 
                                     `(lambda (,x : ,(type-lookup type-obs x T)) ,(final-type env e))]
            
            
            
            
            )))

;--------------------------------------------- tests ----------------------------------------------------
;
;
;(define start (string-append "\n" "--------------------- start --------------------" "\n"))
;(define end (string-append "\n" "--------------------- end --------------------" "\n"))
;"test 1"
;start
;
;(define test1 (typecheck '() '(lambda (x1) (inc x1 l1))))
;test1
;(display "evaluation --> ")
;(evac '() `(,test1 3))
;(define re1 (final-type '() test1))
;re1
;""
;"----------------typecheck again---------------\n"
;(typecheck '() re1)
;
;end
;"test 2"
;start
;
;(define test2 (typecheck '() '(lambda (x2) (lambda (y2) (inc x2 l2)))))
;test2
;(display "evaluation --> ")
;(evac '() `((,test2 3)4))
;(define re2 (final-type '() test2))
;re2
;""
;"----------------typecheck again---------------"""
;(typecheck '() re2)
;
;end
;"test 3"
;start
;
;(define test3 (typecheck '() '(lambda (x3) (lambda (y3) (inc x3 l3)))))
;test3
;(display "evaluation --> ")
;(evac '() `((,test3 3)4))
;(define re3 (final-type '() test3))
;re3
;""
;"----------------typecheck again---------------"""
;(typecheck '() re3)
;
;end
;"test 4"
;
;start
;
;(define test4 (typecheck '() '(lambda (x4) (lambda (y4) (lambda (z4) (dec (dec x4 l4) l42))))))
;test4
;(display "evaluation --> ")
;(evac '() `(((,test4 3)#t)4))
;(define re4 (final-type '() test4))
;re4
;""
;"----------------typecheck again---------------" ""
;(typecheck '() re4)
;
;end
;
;"test 5"
;
;start
;
;(define test5 (typecheck '() '(lambda (x5) (lambda (y5) (lambda (z5) (zero? (dec (dec x5 l5) l52) l53))))))
;test5
;
;(display "evaluation --> ")
;(evac '() `(((,test5 3)#f)#t))
;(define re5 (final-type '() test5))
;re5
;""
;"----------------typecheck again---------------"""
;(typecheck '() re5)
;          
          

;(define test7 (typecheck '() '(((lambda (x1 : dyn) (#t : dyn l1)) : (-> int int) l2) 42 l3)))

;test7

;(evac '() test7)

;(define test6 (typecheck '() '((lambda (x2 : dyn) x2) 42 l1 )))
;test6
;(evac '() test6)

(define test8 (typecheck '() '((lambda (x : dyn) (x 42 l2)) (lambda (y : int) y) l1)))


(evac '() (car test8))

;(final-type '() '((call (cast l2 (lambda (x : dyn) (cast ;l1 2 : int -> dyn)) : (-> dyn dyn) -> (-> int int)) 42) int))

(define test10 (typecheck '() '((lambda (x1 : int) ((lambda (y1 : int)y1) x1 l1))10 l2)))
test10
(evac '() (car test10))
