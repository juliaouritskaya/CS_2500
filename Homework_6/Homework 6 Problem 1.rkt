;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6 Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: In this problem (and all subsequent problems), for every function
;; design, you must write (1) *at least* two tests, and (2) ensure that there
;; are no "halloween colors". To address the latter, you may have to write
;; additional tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the three functions below (we have deliberately omitted tests):

;; hello-everyone: [List-of String] -> [List-of String]
;; Greets everyone in the list.

(check-expect
 (hello-everyone '())
 '())

(check-expect
 (hello-everyone (cons "Julia" '()))
 (cons "Hello, Julia!" '()))

(check-expect
 (hello-everyone (cons "Julia" (cons "Isha" '())))
 (cons "Hello, Julia!" (cons "Hello, Isha!" '())))

(define (hello-everyone los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello, " (first los) "!")
                       (hello-everyone (rest los)))]))

;; words-until-period: [List-of String] -> [List-of String]
;; Produces the words in the list up to the first period.

(check-expect
 (words-until-period '())
 '())
 
(check-expect
 (words-until-period (cons "." '()))
 '())

(check-expect
 (words-until-period (cons "hello" (cons "." (cons "hi" '()))))
 (cons "hello" '()))
 
#;   (define (words-until-period los)
       (cond
         [(empty? los) '()]
         [(cons? los) 
          (if (string=? (first los) ".")
              '()
              (cons (first los) (words-until-period (rest los))))]))

(define (words-until-period los)
  (starting-value los "." string=?))

;; starting-positive-numbers : [List-of Number] -> [List-of Number]
;; Produces the prefix of positive numbers in the list.

(check-expect
 (starting-positive-numbers '())
 '())

(check-expect
 (starting-positive-numbers (cons -6 (cons 4 '())))
 '())

(check-expect
 (starting-positive-numbers (cons 3 (cons -2 (cons 7 '()))))
 (cons 3 '()))

#;   (define (starting-positive-numbers lon)
       (cond
         [(empty? lon) '()]
         [(cons? lon)
          (if (<= (first lon) 0)
              '()
              (cons (first lon) (starting-positive-numbers (rest lon))))]))

(define (starting-positive-numbers lon)
  (starting-value lon 0 <=))

;; Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

;; [TODO] Design a list abstraction.

;; starting-value : (X) [Listof X] X [X -> Boolean] -> [List of X]
;; Given a list of elements of data type X, a desired value X, and a function that
;; checks if X satisfies a condition, produces a new list of data type X.

(check-expect
 (starting-value '() "." string=?)
 '())

(check-expect
 (starting-value (cons "hello" (cons "." (cons "hi" '()))) "." string=?)
 (cons "hello" '()))

(check-expect
 (starting-value '() 0 <=)
 '())

(check-expect
 (starting-value (cons -6 (cons 4 '())) 0 <=)
 '())

(check-expect
 (starting-value (cons 3 (cons -2 (cons 7 '()))) 0 <=)
 (cons 3 '()))

(define (starting-value lox desired func)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (if (func (first lox) desired)
         '()
         (cons (first lox) (starting-value (rest lox) desired func)))]))

;; Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can.


