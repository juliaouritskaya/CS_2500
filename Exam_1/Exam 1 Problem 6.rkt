;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exam 1 Problem 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; Consider the following data definition that represents the available letters
;; in a simplified game of Spelling Bee (identical to the project you've worked
;; on before).

(define-struct letters [center left right top bottom])
;; A Letters is a (make-letters 1String 1String 1String 1String 1String)
;; Interpretation: the letters available in a game of Spelling Bee:
;; - center: the letter at the center that must be used
;; - left, right, top, bottom: the four other letters

;; Examples:
(define LETTERS-1 (make-letters "Q" "U" "I" "T" "S"))
(define LETTERS-2 (make-letters "X" "Y" "Z" "Q" "T"))
(define LETTERS-3 (make-letters "A" "B" "C" "D" "E"))

;; Template: 
(define (letters-template l)
  (... (letters-center l) ...
       (letters-left l) ...
       (letters-right l) ...
       (letters-top l) ...
       (letters-bottom l) ...))

;; The letter "Q" is typically followed by the letter "U". So, it would be
;; very unfortunate if the available letters had a "Q" and *not* a "U".
;; Design a function that produces #false when a Letters has a "Q" but
;; omits a "U".

;; Ensure you follow the complete function design recipe, including for any
;; appropriate helper functions you may choose to write.

;; [TODO] Function design

;; has-q? : Letters -> Boolean
;; Determines if Letters has a "Q".

(check-expect (has-q? LETTERS-1) #true)
(check-expect (has-q? LETTERS-2) #true)
(check-expect (has-q? LETTERS-3) #false)

(define (has-q? l)
  (cond
    [(string=? (letters-center l) "Q") #true]
    [(string=? (letters-left l) "Q") #true]
    [(string=? (letters-right l) "Q") #true]
    [(string=? (letters-top l) "Q") #true]
    [(string=? (letters-bottom l) "Q") #true]
    [else #false]))

;; has-u? : Letters -> Boolean
;; Determines if Letters has a "U".

(check-expect (has-u? LETTERS-1) #true)
(check-expect (has-u? LETTERS-2) #false)
(check-expect (has-u? LETTERS-3) #false)

(define (has-u? l)
  (cond
    [(string=? (letters-center l) "U") #true]
    [(string=? (letters-left l) "U") #true]
    [(string=? (letters-right l) "U") #true]
    [(string=? (letters-top l) "U") #true]
    [(string=? (letters-bottom l) "U") #true]
    [else #false]))

;; has-q-no-u? : Letters -> Boolean
;; Produces #false when Letters has a "Q" but omits a "U".
             
(check-expect (has-q-no-u? LETTERS-1) #true)
(check-expect (has-q-no-u? LETTERS-2) #false)
(check-expect (has-q-no-u? LETTERS-3) #true)

(define (has-q-no-u? l)
  (cond
    [(and (has-q? l)
          (has-u? l)) #true]
    [(and (has-q? l)
          (not (has-u? l))) #false]
    [(and (not (has-q? l))
          (not (has-u? l))) #true])) 
 

