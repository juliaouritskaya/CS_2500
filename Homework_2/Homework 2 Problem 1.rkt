;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Design a function that consumes two strings and produces a single string
;; that concatenates them, with a space between them. However, in this result,
;; the first string should be the *longer* of the two. Ensure you follow
;; all steps of the design recipe, and include three distinct check-expects.

;; [TODO]

;; string-adder : String String -> String
;; Consumes two strings and produces a single string that concatenates them,
;; with a space between them, appending the shorter string to the longer one.

(check-expect (string-adder "beautiful" "day") "beautiful day")
(check-expect (string-adder "computer" "world") "computer world")
(check-expect (string-adder "hello" "world") "world hello")

(define (string-adder string1 string2)
  (if (> (string-length string1) (string-length string2))
      (string-append string1 " " string2)
      (string-append string2 " " string1)))
  


