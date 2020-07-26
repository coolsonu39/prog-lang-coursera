;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1 (warm up)
(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (aunit)]
        ; apair is like cons for MUPL
        [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

; similar to above, just other way around
(define (mupllist->racketlist mlist)
  (cond [(aunit? mlist) null]
        ; apair-e1 -> head(car), apair-e2 -> tail(cdr)
        [#t (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))]))

;; Problem 2 (implementing the MUPL language)

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) ; already given
         (envlookup env (var-string e))]
        [(add? e) ; already given
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        ; ALL VALUES FIRST (evaluate to themselves), my code
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]

        ; functions evaluate to closure holding the function & current environment
        ; (lexical scope)
        [(fun? e) (closure env e)]

                         ; evaluate first two subexpressions to v1 & v2
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          ; check the subexpressions
                          (if (and (int? v1) (int? v2))
                              ; v1 strictly greater than v2?
                              (if (> (int-num v1) (int-num v2))
                                  (eval-under-env (ifgreater-e3 e) env)
                                  (eval-under-env (ifgreater-e4 e) env))
                              (error "subexpressions of ifgreater are not integers")))]

                   ; evaluate first expression to a value v
        [(mlet? e) (let ([v (eval-under-env (mlet-e e) env)])
                                                   ; extended env with mlet expression v
                     (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]

                   ; evaluate 1st & 2nd sub-expressions to values
        [(call? e) (let ([clsr (eval-under-env (call-funexp e) env)]
                         [arguments (eval-under-env (call-actual e) env)])
                     ; if first is closure evaluate func. body (extended env), else error
                     (if (closure? clsr)
                         (letrec ([function (closure-fun clsr)]
                                  [environment (closure-env clsr)]
                                  [extended (cons (cons (fun-formal function) arguments) environment)]
                                  ; for anonymous functions?
                                  [ext-env (if (fun-nameopt function)
                                               (cons (cons (fun-nameopt function) clsr) extended)
                                               extended)])
                           (eval-under-env (fun-body function) ext-env))
                         (error "not a closure")))]

                    ; evaluate two sub expressions
        [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                      (apair v1 v2))] ; produce a new pair holding the results

                  ; evaluate its subexpression
        [(fst? e) (let ([v (eval-under-env (fst-e e) env)])
                    ; if v is pair then e1 else error
                    (if (apair? v) (apair-e1 v) (error "e is not a pair")))]

        ; similar to fst
        [(snd? e) (let ([v (eval-under-env (snd-e e) env)])
                    (if (apair? v) (apair-e2 v) (error "e is not a pair")))]

        [(isaunit? e) (let ([v (eval-under-env (isaunit-e e) env)]) ; evaluate subexpression
                        (if (aunit? v) (int 1) (int 0)))]              
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3 (expanding the language)

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) (if (null? lstlst) e2 (mlet (car (car lstlst))
                                                      (cdr (car lstlst))
                                                      (mlet* (cdr lstlst) e2))))
                                                   
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4 (using the laguage)

(define mupl-map
  (fun "mupl-map" "map-fun"
       (fun #f "map-lst"
            (ifaunit (var "map-lst")
                     (aunit)
                     (apair (call (var "map-fun") (fst (var "map-lst")))
                            (call (call (var "mupl-map") (var "map-fun"))
                                  (snd (var "map-lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem, will do later

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
