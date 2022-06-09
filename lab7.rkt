#lang typed/racket
(require typed/rackunit)

;; represents an expression
(define-type ExprC (U Real Symbol String LamC AppC IfC SetC))
(struct LamC ([params : (Listof Symbol)]
              [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC]
              [args : (Listof ExprC)]) #:transparent)
(struct IfC ([test : ExprC]
             [thn : ExprC]
             [els : ExprC]) #:transparent)
(struct SetC ([var : Symbol]
              [newval : ExprC]))
(define-type PrimV (U '+ '- '* '/))


;; represents a value
(define-type Value (U Real Boolean String CloV PrimV
                      ArrayV))
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Env]))
(struct ArrayV ([addr : Address] [length : Natural])
  #:transparent)


;; represents an Environment
(define-type Env (HashTable Symbol Address))
 
;; represents an address
(define-type Address Natural)
(define-type Addresses (Listof Address))

;; represents a store
(define-type Store (Mutable-HashTable Address Value))

;accepts a value and computes a list of the memory locations it refers to
(define (findLocationsE [e : Env]) : Addresses
  (cast (hash-values e) Addresses))

;accepts an environment and computes a list of the memory locations it refers to
(define (findLocationsV [v : Value]) : Addresses
  (match v
    [(ArrayV addr length) (build-list (cast length Integer) (lambda ([x : Index]) (+ x addr)))]
    [(CloV params body env) (findLocationsE env)]
    [other '()]
    )
  )

;return a list of all the memory locations that are referred to directly or indirectly in unexamined
(define (mark [seen : Addresses] [unexamined : Addresses] [sto : Store]) : Addresses
  (match unexamined
    ['() '()]
    [(cons f r)
     (define foundLocations (findLocationsV (hash-ref sto (first unexamined))))
     (define newUnexamined (filter-not (lambda (x) (member x seen)) (append foundLocations (rest unexamined))))
     (append (list f) foundLocations (mark (cons (first unexamined) seen) newUnexamined sto))]))

;accepts a store and a list of still-used locations, and returns a list of the no-longer-referred-to locations
(define (sweep [sto : Store] [used : Addresses]) : Addresses
  (filter-not (lambda (x) (member x used)) (hash-keys sto)))

;determine what addresses in the store are unused
(define (collect [e : Env] [sto : Store]) : Addresses
  (sweep sto (mark '() (hash-values e) sto)))



(define testEnv (cast (make-hash (list
                                  (cons 'a 6)
                                  (cons 'b 7))) Env))
(define testEnv2 (cast (make-hash (list
                                  (cons 'a 6)
                                  (cons 'b 7)
                                  (cons 'arr 3))) Env))
(define testEnv3 (cast (make-hash (list
                                  (cons 'a 6)
                                  (cons 'b 7)
                                  (cons 'c 4)
                                  (cons 'arr 3))) Env))

(define testEnv0 (cast (make-hash (list
                                  (cons 'x 11)
                                  (cons 'y 12)
                                  (cons 'z 13))) Env))

(define testSto (cast (make-hash (list
                                  (cons 0 1)
                                  (cons 1 2)
                                  (cons 2 3)
                                  (cons 3 (ArrayV 0 3))
                                  (cons 4 (CloV '() 0 testEnv0))
                                  (cons 5 #t)
                                  (cons 6 #f)
                                  (cons 7 #t)
                                  (cons 8 #f)
                                  (cons 9 0)
                                  (cons 10 4)
                                  (cons 11 5)
                                  (cons 12 6)
                                  (cons 13 7)
                                  (cons 14 8)
                                  )) Store))

(define testSto2 (cast (make-hash (list
                                  (cons 0 1)
                                  (cons 1 (ArrayV 8 2))
                                  (cons 2 3)
                                  (cons 3 (ArrayV 0 3))
                                  (cons 4 (CloV '() 0 testEnv0))
                                  (cons 5 #t)
                                  (cons 6 #f)
                                  (cons 7 #t)
                                  (cons 8 #f)
                                  (cons 9 0)
                                  (cons 10 4)
                                  (cons 11 5)
                                  (cons 12 6)
                                  (cons 13 7)
                                  (cons 14 8)
                                  )) Store))

(define testSto3 (cast (make-hash (list
                                  (cons 0 1);
                                  (cons 1 (ArrayV 3 1));  @1 : Array holding 3 -> add 1 to seen, 3 to unexamined
                                  (cons 2 3);
                                  (cons 3 (ArrayV 0 3));  @3 : Array holding 0 1 2 -> add 3 to seen, 0 1 2 to unexamined
                                  (cons 4 (CloV '() 0 testEnv0))
                                  (cons 5 #t)
                                  (cons 6 #f)
                                  (cons 7 #t)
                                  (cons 8 #f)
                                  (cons 9 0)
                                  (cons 10 4)
                                  (cons 11 5)
                                  (cons 12 6)
                                  (cons 13 7)
                                  (cons 14 8)
                                  )) Store))


;'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)

(check-equal? (collect testEnv testSto) '(0 1 2 3 4 5 8 9 10 11 12 13 14))
(check-equal? (collect testEnv2 testSto) '(4 5 8 9 10 11 12 13 14))
(check-equal? (collect testEnv3 testSto) '(5 8 9 10 14))
(check-equal? (collect testEnv3 testSto2) '(5 10 14))
(check-equal? (collect testEnv3 testSto3) '(5 8 9 10 14))
