
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (strElem) (string-append strElem suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

; 4, case with n=0 not included
(define (stream-for-n-steps s n)
  (cond [(= n 1) (cons (car (s)) null)]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cond
                           [(string=? x "dan.jpg") (cons x (lambda () (f "dog.jpg")))]
                           [(string=? x "dog.jpg") (cons x (lambda () (f "dan.jpg")))]))])
    (lambda () (f "dan.jpg"))))

; 7 (mind = blown)
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

; 8 (am i genius?)
(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n) (cons
                                (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (helper (+ n 1)))))])
    (lambda () (helper 0))))

; 9
(define (vector-assoc v  vec)
  (letrec ([helper (lambda (n)
                     (if (<= (vector-length vec) n)
                         #f
                         (let ([i (vector-ref vec n)])
                           (if (and (pair? i) (equal? (car i) v))
                               i
                               (helper (+ n 1))))))])
    (helper 0)))

; 10, similar to factorial3?
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)] ; cache of length n
           [loc 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([ans (assoc v xs)])
            (and ans
                 (begin
                   (vector-set! memo loc ans) ; update memo
                   (set! loc (remainder (+ loc 1) n))
                   ans)))))))

; 11 challenge problem, will do later
; while-less
