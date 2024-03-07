;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 9 Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; NOTE #1: You may use list abstractions if you wish. However, they are not
;; mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists of numbers and produces a list of
;; numbers where each number is the average of the two corresponding numbers in 
;; the original lists. You may assume that the two lists have equal length.

;; [TODO]

;; average-all : [Listof Number] [Listof Number] -> [Listof Number]
;; Produces a list of numbers where each number is the average of the
;; two corresponding numbers in the original given lists.

(check-expect
 (average-all '() '())
 '())

(check-expect
 (average-all (list 1 2) (list 3 4))
 (list 2 3))

(define (average-all lon1 lon2)
  (cond
    [(empty? lon1) '()]
    [(cons? lon1)
     (cons (average (first lon1) (first lon2))
           (average-all (rest lon1) (rest lon2)))]))

;; average : Number Number -> Number
;; Produces the average of two given numbers.

(check-expect
 (average 1 3)
 2)

(check-expect
 (average 3 5)
 4)

(define (average n1 n2)
  (/ (+ n1 n2) 2)) 




