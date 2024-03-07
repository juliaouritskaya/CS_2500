;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: For all function designs, you must have 2+ tests and no halloween
;; colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; The final objective in this problem is to design these three functions:
;;
;; 1. hello-goodbye: given a list of names, produces two strings for each
;;    string NAME: "Hello NAME!" and "Goodbye NAME!".
;; 2. double-double: given a list of numbers, produces a list with two numbers
;;    for each number x: (* 2 x) and (* 4 x).
;; 3. string-length-length : given a list of strings, produces a list with two 
;;    numbers for each string: the length, followed by half the length.
;;
;; However, you must not use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.

;; To help you get started, here are the tests for the three functions.
;; However, you should also test your abstraction independently.


;; double-all : (X Y) [Listof X] [X -> Y] [X -> Y] -> [Listof Y]
;; Given a list of elements of data type X and two functions that change
;; the elements into data type Y, returns a new list of Y values.

(check-expect
 (double-all '() append-hello append-goodbye)
 '())

(check-expect
 (double-all (cons "Alice" (cons "Bob" '())) append-hello append-goodbye)
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))

(check-expect
 (double-all '() times-two times-four)
 '())

(check-expect
 (double-all (cons 10 (cons 20 '())) times-two times-four)
 (cons 20 (cons 40 (cons 40 (cons 80 '())))))

(check-expect
 (double-all '() string-length string-length/2)
 '())

(check-expect
 (double-all (cons "Hello" '()) string-length string-length/2)
 (cons 5 (cons 2.5 '())))

(define (double-all lox func1 func2)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (cons (func1 (first lox))
           (cons (func2 (first lox))
                 (double-all (rest lox) func1 func2)))]))


;; hello-goodbye : [Listof String] -> [Listof String]
;; Given a list of names, produces two strings for each
;; string NAME: "Hello NAME!" and "Goodbye NAME!".

(check-expect (hello-goodbye '()) '())

(check-expect
 (hello-goodbye (cons "Alice" (cons "Bob" '())))
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))

#;    (define (hello-goodbye los)
        (cond
          [(empty? los) '()]
          [(cons? los)
           (cons (append-hello (first los))
                 (cons (append-goodbye (first los))
                       (hello-goodbye (rest los))))]))

(define (hello-goodbye los)
  (double-all los append-hello append-goodbye))

;; append-hello : String -> String
;; Given a name, produces "Hello name!".

(check-expect (append-hello "Alice") "Hello Alice!")

(define (append-hello name)
  (string-append "Hello " name "!"))

;; append-goodbye : String -> String
;; Given a name, produces "Goodbye name!".

(check-expect (append-goodbye "Alice") "Goodbye Alice!")

(define (append-goodbye name)
  (string-append "Goodbye " name "!"))


;; double-double : [Listof Number] -> [Listof Number]
;; Given a list of numbers, produces a list with two numbers
;; for each number x: (* 2 x) and (* 4 x).

(check-expect (double-double '()) '())

(check-expect (double-double (cons 10 (cons 20 '())))
              (cons 20 (cons 40 (cons 40 (cons 80 '())))))

#;    (define (double-double lon)
        (cond
          [(empty? lon) '()]
          [(cons? lon)
           (cons (times-two (first lon))
                 (cons (times-four (first lon))
                       (double-double (rest lon))))]))

(define (double-double lon)
  (double-all lon times-two times-four))

;; times-two : Number -> Number
;; Multiples a number by 2.

(check-expect (times-two 2) 4)

(define (times-two n)
  (* n 2))

;; times-four : Number -> Number
;; Multiples a number by 4.

(check-expect (times-four 2) 8)

(define (times-four n)
  (* n 4))


;; string-length-length : [Listof String] -> [Listof Number]
;; Given a list of strings, produces a list with two 
;; numbers for each string: the length, followed by half the length.

(check-expect (string-length-length '()) '())

(check-expect (string-length-length (cons "Hello" '()))
              (cons 5 (cons 2.5 '())))

#;    (define (string-length-length los)
        (cond
          [(empty? los) '()]
          [(cons? los)
           (cons (string-length (first los))
                 (cons (string-length/2 (first los))
                       (string-length-length (rest los))))]))

(define (string-length-length los)
  (double-all los string-length string-length/2))

;; string-length/2 : String -> Natural
;; Produces half the length of a given string.

(check-expect (string-length/2 "Beauty") 3)

(define (string-length/2 s)
  (/ (string-length s) 2))


