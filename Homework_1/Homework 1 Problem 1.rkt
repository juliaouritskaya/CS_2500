;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1 Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Part A

;; Examine the following function, and:
;; i.   Write down its signature,
;; ii.  Give it an informative purpose statement, and
;; iii. Give the function and its arguments more informative names.

;; [TODO] Signature
;; [TODO] Purpose

;; detective : Number Number String -> String
;; Determines if the person can board the ride
;; based on their height.

(define (detective a b c)
  (if (< (+ (* 12 a) b) 48)
      (string-append "Welcome aboard, " c "!")
      "Sorry, you may not board the ride. :("))

;; [TODO] Better names for detective and its arguments

;; height-for-ride : Number Number String -> String
;; Determines if the person can board the ride
;; based on their height.

(define (height-for-ride feet inches name)
  (if (< (+ (* 12 feet) inches) 48)
      (string-append "Welcome aboard, " name "!")
      "Sorry, you may not board the ride. :("))


;; Part B

;; Write the signature of the following function:

;; [TODO] Signature

;; mystery : String String Number -> String
;; Concatenates sections of two words based on the number given.

(define (mystery x y z)
  (string-append (substring x 0 z)
                 (substring y z)
                 (substring y 0 z)
                 (substring x z)))

;; Part C

;; Describe the values that mystery produces when you apply it to two identical
;; arguments for x and y.


;; [TODO] Prose description as a comment.

;; When you apply mystery to two identical arguments for x and y, it produces the same word twice,
;; i.e., x appended to y. This is because the substring function takes the characters of x up to
;; the z place and appends it to the charcters of y, beggining from the z place until the end of
;; the word. The same occurs for y. 
;; For example:

(mystery "hello" "hello" 2)

;; produces hellohello


