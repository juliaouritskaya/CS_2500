;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam 2 Problem 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;; Consider the following data definition:

(define-struct person [name parent1 parent2])
;; A Person is one of:
;; - #false
;; - (make-person String Person Person)
;; Interpretation:  A person's name and two biological parents.
;; Or a sentinel value representing that a person's identity/lineage is not 
;; known.

(define (person-template p)
  (cond
    [(boolean? p) ...]
    [(person? p)
     (... (person-name p) ...
          (person-temp (person-parent1 p)) ...
          (person-temp (person-parent2 p)) ...)]))

(define PERSON-0 #false)
(define PERSON-1 (make-person "Alice" PERSON-0 PERSON-0))
(define PERSON-2 (make-person "Bob" PERSON-0 PERSON-0))
(define PERSON-3 (make-person "Carol" PERSON-1 PERSON-2))
(define PERSON-4 (make-person "David" PERSON-3 (make-person "Erin" PERSON-0 PERSON-0)))

;; Design a function called remove-ancestors that receives a Person and a
;; list of names (strings) to remove, and produces a Person without those names.
;; If a name in the list has ancestors, all their ancestors will be removed
;; as well.
;;
;; For example, here is a check-expect that should succeed. You should write
;; at least one more.

(check-expect
 (remove-ancestors PERSON-4 (list "Erin" "Bob"))
 (make-person "David" 
              (make-person "Carol" PERSON-1 #false)
              #false))

;; remove-ancestors : Person [Listof String] -> Person
;; Produces a person without the supplied list of names.

(check-expect
 (remove-ancestors PERSON-3 (list "Alice"))
 (make-person "Carol" #false PERSON-2))

(define (remove-ancestors p los)
  (local [; remove-person : Person -> Person
          ; Produces a person without supplied names.
          (define (remove-person p los)
            (cond
              [(empty? los) p]
              [(cons? los)
               (if (string=? (person-name p) (first los))
                   #false
                   p)]))]
    (cond 
      [(boolean? p) #false]
      [(person? p)
       (make-person (person-name p)
                    (remove-person (person-parent1 p) los)
                    (remove-person (person-parent2 p) los))])))  

