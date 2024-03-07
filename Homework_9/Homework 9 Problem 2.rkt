;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 9 Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; NOTE #3: You *may not* use the builtin function replicate to solve this
;; problem.

;; Design a function that receives a number N and a list of strings, and
;; produces a list of strings where each output string is the corresponding
;; input string repeated N times.

;; [TODO] Function design *without using replicate*

;; repeat-all : Nat [Listof String] -> [Listof String]
;; Produces a list of strings where each output string is the
;; corresponding input string repeated the given amount of times.

(check-expect
 (repeat-all 0 '())
 '())

(check-expect
 (repeat-all 2 (list "hi" "hey"))
 (list "hihi" "heyhey"))

(define (repeat-all num los)
  (cond
    [(or (= num 0) (empty? los)) '()]
    [(and (> num 0) (cons? los))
     (cons (repeat num (first los))
           (repeat-all num (rest los)))]))

;; repeat : Nat String -> String
;; Repeats a string based on the given number.

(check-expect
 (repeat 0 "hi")
 "")

(check-expect
 (repeat 2 "hi")
 "hihi")

(define (repeat num s)
  (foldr string-append
         ""
         (build-list num (λ (_) s))))


