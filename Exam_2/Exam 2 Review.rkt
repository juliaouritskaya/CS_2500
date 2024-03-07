;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam 2 Review|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Lambdas and Locals

;; Lambdas
;; Really good for very simple functions
;; One line at most (usually)
;; They don't follow the design recipe
;; They don't have names

;; Format (λ (arg1 arg2 ...) (function-body))
;; Example (λ (x y) (string=? x y))

;; They're really great with list abstractions

;; Example: add 3 to all items in a list of numbers

;; add3/lambda : [Listof Number] -> [Listof Number]
;; Adds 3 to all items in a list of numbers using lambda.

(check-expect (add3/lambda '()) '())
(check-expect (add3/lambda (list 1 2)) (list 4 5))

(define (add3/lambda lon)
  (map (λ (n) (+ 3 n)) lon))

;; Locals
;; Locals allow you to make more complex helper functions
;; Locals allow you to define constants in you function

;; These functions follow most parts of the design recipe
;; Signature and purpose

;; Format: (local [(define CONSTANT (...)
;;                  Signature and purpose
;;                 (define (function/local a) (...)]
;;                  (call things from local))

;; add3/local : [Listof Number] -> [Listof Number]
;; Adds 3 to all items in a list of numbers using local.

(check-expect (add3/local '()) '())
(check-expect (add3/local (list 1 2)) (list 4 5))

(define (add3/local lon)
  (local [; add3 : Number -> Number
          ; Adds 3 to a number.
          (define (add3 n)
            (+ 3 n))]
    (map add3 lon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List Abstractions

;; map : (X Y) [X -> Y] [Listof X] -> [Listof Y]
;; I want to do something to every element in the list.

;; andmap : (X) [X -> Boolean] [Listof X] -> Boolean
;; I want to know if some predicate is true for EVERY element in the list.

;; ormap : (X) [X -> Boolean] [Listof X] -> Boolean
;; I want to know if some predicate is true for AT LEAST ONE element in the list.

;; filter : (X) [X -> Boolean] [Listof X] -> [Listof X]
;; I want only some elements in this list.
;; I only want the elements that meet some requirement/predicate.

;; foldr : (X Y) [X Y -> Y] Y [Listof X] -> Y
;; Starts with the last element of the list.
;; foldl : (X Y) [X Y -> Y] Y [Listof X] -> Y
;; Starts with the first element of the list.
;; Most flexible of list abstractions, change [Listof X] to something new.

;; sort : (X) [Listof X] [X X -> Boolean] -> [Listof X]
;; Sorts the list by some predicate.

;; build-list : (X) Nat [Nat -> X] -> [Listof X]
;; I want to create a list of size n.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Design Recipe
;; 1. Data definition
;; 2. Purpose
;; 3. Examples
;; 4. Template
;; 5. Code
;; 6. Tests

;; What do you do when you need to process 2 lists at the same time?
;; Use the template!!

(define (2d-list-temp l1 l2)
  (...
   (cond
     [(and (empty? l1) (empty? l2)) ...]
     [(and (cons? l1) (empty? l2)) ...]
     [(and (empty? l1) (cons? l2)) ...]
     [(and (cons? l1) (cons? l2)) ...])))

;; 3 General Types:

;; 1. Sequential: Process all the items in the first list, then the second.
;; Example: append

(check-expect (my-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

(define (my-append l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) l2]
    [(and (cons? l1) (empty? l2))
                 l1]
    [(and (empty? l1) (cons? l2)) l2]
    [(and (cons? l1) (cons? l2))
                (cons (first l1) (my-append (rest l1) l2))]))

;; 2. Parallel: Process one item from each list at a time.
;; Example: average

(check-expect (average (list 1 2) (list 3 4)) (list 2 3))

(define (average l1 l2)
  (cond
    [(empty? l1) '()]
    [(cons? l1)
     (cons
      (/ (+ (first l1) (first l2)) 2)
      (average (rest l1) (rest l2)))]))

;; 3. Cross-Product: Process each item in one list using all/some of the items in the second.

(check-expect (multiply-sums (list 1 2 3) (list 1 2 3)) 36)
;; because (1 * 6) + (2 * 6) + (3 * 6) = 36

(define (multiply-sums l1 l2)
  (cond
    [(empty? l1) 0]
    [(cons? l1)
     (+ (* (first l1) (foldr + 0 l2))
        (multiply-sums (rest l1) l2))]))































