;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam 2 Review Problm 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 2, Problem 1 ==

; TODO #1: design the function pair-then-smash that takes two lists (assumed to be the same
; length), performs one function on each pair from the two lists (the first from each list,
; then the second, ...), and then applies another function that takes the list of pairwise
; results and produces a final outcome.

; For example, consider the following parallel lists of strings and natural numbers...

(define L1 (list "a" "bb" "cccc"))
(define L2 (list 1 2 3))

; now consider the following function that determines if the length of a supplied string
; is equal to a supplied number...

(define (fp1 s n)
  (= (string-length s) n))

; applying this pairwise across L1 and L2 means (fp1 "a" 1), then (fp1 "bb" 2), ...
; and producing a result list...

(define PAIR-12 (list #true #true #false))

; so then consider the following function that asks if all the results are true...

(define (fs1 l)
  (andmap identity l))

; producing an overall answer (#false) that, in fact, one of the strings length is NOT
; equal to its corresponding number.

; For another example, consider the result of adding each pairwise element of L2
; with itself...

(define PAIR-22 (list 2 4 6))

; and then take the product of this resulting list...

(define (fs2 l)
  (foldr * 1 l))

; producing the answer of 48.
; For clarity, these examples have been supplied as the following tests;
; you can create additional tests but are not required to do so.

(check-expect
 (pair-then-smash L1 L2 fp1 fs1)
 #false)

(check-expect
 (pair-then-smash L2 L2 + fs2)
 48)

; and for good measure...

(check-expect
 (pair-then-smash '() '() fp1 fs1)
 #true)

(check-expect
 (pair-then-smash '() '() + fs2)
 1)

; Note: you may choose to implement the function with list abstractions, template(s), or a
; combination; any well-designed design can receive full credit.

;; pair-then-smash : [Listof X] [Listof Y] [X Y -> Z] [[Listof X] -> R] -> R
;; Applies a function to pairs of elements in each list,
;; takes the resulting list, and applies a function to that.

#; (define (pair-then-smash l1 l2 fp fs)
  (fs (map fp l1 l2)))

;; pair-then-smash : [Listof X] [Listof Y] [X Y -> Z] [[Listof X] -> R] -> R
;; Applies a function to pairs of elements in each list,
;; takes the resulting list, and applies a function to that.

(define (pair-then-smash l1 l2 fp fs)
  (local [; smash : [Listof X] [Listof Y] -> [Listof Z]
          ;; Maps the lists using our pair function. 
          (define (smash lox loy)
            (cond
              [(empty? lox) '()]
              [(cons? lox)
               (cons
                (fp (first lox) (first loy))
               (smash (rest lox) (rest loy)))]))]
    (fs (smash l1 l2))))  

; TODO #2: using pair-then-smash, design the function palindrome? that accepts a list
; and determines if its elements are the same (via a supplied equality predicate) forward
; and backwards. Example tests have been included for clarity; you can create additional
; tests but are not required to do so. As with the previous problem, you are free to design
; your function (well) as you see fit as long as it is making effective use of the
; pair-then-smash abstraction.


(check-expect
 (palindrome? '() =)
 #true)

(check-expect
 (palindrome? (list 1 2) =)
 #false)

(check-expect
 (palindrome? (list 1 2 2 1) =)
 #true)

(check-expect
 (palindrome? (list 1 2 3 2 1) =)
 #true)

(check-expect
 (palindrome? (explode "level") string=?)
 #true)

(check-expect
 (palindrome? (explode "howdy!") string=?)
 #false)

(check-expect
 (palindrome?
  (explode "howdy")
  (λ (s1 s2) (boolean=? (string=? s1 "w") (string=? s2 "w"))))
 #true)

;; palindrome? : (X) [Listof X] [X X -> Boolean] -> Boolean
;; Determines if the elements in the list are the same forward and backwards.

(define (palindrome? lox eq?)
  (pair-then-smash lox (reverse lox) eq? fs1))



