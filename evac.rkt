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
            [`,other var]
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
          
          [`(dec ,e) (guard (integer? (env-lookup env e)))
                            (let [(e1 (evac env (env-lookup env e)))]
                              (- e1 1))]
          
          [`(zero? ,e) (guard (integer? (env-lookup env e)))
                              (let [(e1 (evac env (env-lookup env e)))]
                                (zero? e1))]
          
          
          [`(cast ,label ,e : ,T1 -> ,T2) (extend-hash type-obs e (meet T1 T2)) e]
          
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
          
          [`((lambda (,x : ,T1) ,e ) ,T) (pmatch T
                                                 [`(-> ,T2 ,T3) (evac env e) (evac env `(lambda (,x : ,T1) ,e)) ]
                                                 )]
          
          [`(lambda (,x : ,T1) ,e ) (extend-hash type-obs x T1 ) `(lambda (,x : ,T1) ,(evac env e))]
          
          [`(,e1 ,e2) ;(display `(e1 ,e1 e2 ,e2)) 
                      (let ([v1 (evac env e1)]
                            [v2 (evac env e2)])
                       ;(display `(v1 ,v1 v2 ,v2))
                      (pmatch v1
                              [`(lambda (,x : ,T1) ,e ) ;(display 'aqui)
                                                        (extend-hash type-obs x (meet T1 (typeof v2)))
                                                         (evac (extend-env env x v2) e)
                                                         
                                                         ]
                              [`,other `(,v1 ,v2)]))]
          [`,var var]
          
          )
          )
          

(define final-type
  (lambda (env exp)
    (pmatch exp 
            [`(cast ,label ,e : ,T1 -> ,T2) e]
            [`(prim ,op ,e) (if (member op '(inc zero? dec))
                                `(,op ,(evac env e))
                                (error "operator does not exist"))]
            [`((lambda (,x : ,T1) ,e ) ,T) (pmatch T
                                                 [`(-> ,T2 ,T3) `((lambda (,x : ,(type-lookup type-obs x T1)) ,(final-type env e)),T)]
                                                 )]
            [`(lambda (,x : ,T) ,e ) ;(display 'd) 
                                     `(lambda (,x : ,(type-lookup type-obs x T)) ,(final-type env e))]
            
            
            
            
            )))

;--------------------------------------------- tests ----------------------------------------------------


(define start "----- start ------")
(define end "----- end ------")
"test 1"
start

(define test1 (typecheck '() '(lambda (x1) (inc x1 l1))))
test1
(display "evaluation --> ")
(evac '() `(,test1 3))
(final-type '() test1)
end
"test 2"
start

(define test2 (typecheck '() '(lambda (x2) (lambda (y2) (inc x2 l2)))))
test2
(display "evaluation --> ")
(evac '() `((,test2 3)4))
(final-type '() test2)
end
"test 3"
start

(define test3 (typecheck '() '(lambda (x3) (lambda (y3) (zero? y3 l3)))))
test3
(display "evaluation --> ")
(evac '() `((,test3 #f)4))
(final-type '() test3)

end
"test 4"

start

(define test4 (typecheck '() '(lambda (x4) (lambda (y4) (lambda (z4) (dec (dec x4 l4) l42))))))
test4
(display "evaluation --> ")
(evac '() `(((,test4 3)#t)4))
(final-type '() test4)
end

"test 5"

start

(define test5 (typecheck '() '(lambda (x5) (lambda (y5) (lambda (z5) (zero? (dec (dec x5 l5) l52) l53))))))
test5

(display "evaluation --> ")
(evac '() `(((,test5 3)#f)#t))
(final-type '() test5)       

          
          
          

type-obs

