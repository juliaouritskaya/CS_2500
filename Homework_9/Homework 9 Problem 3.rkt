;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 9 Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists: a list of strings and a list of
;; numbers. The output should be a list of strings, where each string in the
;; output is the corresponding input string duplicated N times, where N
;; is the number in the corresponding list of numbers. However:
;;
;; 1. If there  are more strings than numbers, assume that the extra strings
;;    should be repeated twice each.
;; 1. If there are more numbers than strings, for each extra number N, 
;;    repeat the the string "Extra!" N times.

;; [TODO] Function design, and you *may* use replicate.

;; duplicate-all : [Listof String] -> [Listof Number]
;; Outputs a list of strings, where each string in the output
;; is the corresponding input string duplicated N times,
;; where N is the number in the corresponding list of numbers.

(check-expect
 (duplicate-all '() '())
 '())

(check-expect
 (duplicate-all '()
                (list 3))
 (list "Extra!Extra!Extra!"))

(check-expect
 (duplicate-all (list "goodbye")
                '())
 (list "goodbyegoodbye"))

(check-expect
 (duplicate-all (list "hello" "hi")
                (list 3 5))
 (list "hellohellohello" "hihihihihi"))

(check-expect
 (duplicate-all (list "fundies" "discrete")
                (list 4))
 (list "fundiesfundiesfundiesfundies" "discretediscrete"))

(check-expect
 (duplicate-all (list "coffee")
                (list 2 4))
 (list "coffeecoffee" "Extra!Extra!Extra!Extra!"))

(define (duplicate-all los lon)
  (cond
    [(and (empty? los) (empty? lon)) '()]
    [(and (empty? los) (cons? lon))
     (cons
      (replicate (first lon) "Extra!")
      (duplicate-all '() (rest lon)))]
    [(and (cons? los) (empty? lon))
     (cons
      (replicate 2 (first los))
      (duplicate-all (rest los) '()))]
    [(and (cons? los) (cons? lon))
     (cons
      (replicate (first lon) (first los))
      (duplicate-all (rest los) (rest lon)))])) 












