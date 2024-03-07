;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; Consider the following data definitions and interpretations.

(define-struct sneaker [is-running])
;; A ShoeKind is one of:
;; - (make-sneaker Boolean)
;; - "dress"
;; - "boots"
;; - "other"
;; Interpretation: Represents some shoe varieties. In (make-sneaker r),
;; r is #true when the sneaker is a running shoe. The "other"
;; variety is used for the many other kinds of shoes that aren't listed.

(define-struct shoe [brand laced kind])
;; A Shoe is a (make-shoe String Boolean ShoeKind).
;; Interpretation: A (make-shoe b l k) represents a shoe made by b,
;; of kind k, which has laces when l is #true, and is a slip-on shoe
;; when l is #false.

;; Part A

;; Define five distinct examples of Shoe data. Make sure that your
;; examples are representative, and are distinct enough to cover all the
;; interesting differences in this data.

;; [TODO] Five examples

(define SHOE-1 (make-shoe "Adidas" #true (make-sneaker #true)))
(define SHOE-2 (make-shoe "Nike" #true (make-sneaker #false)))
(define SHOE-3 (make-shoe "Jimmy Choo" #false "dress"))
(define SHOE-4 (make-shoe "Dr. Martens" #true "boots"))
(define SHOE-5 (make-shoe "Crocs" #false "other"))

;; Part B

;; Write templates for all data definitions above.

;; [TODO] Templates

;; Template:
(define (shoekind-temp s)
  (cond
    [(sneaker? s) (... (sneaker-is-running s) ...)]
    [(string=? s "dress") ...]
    [(string=? s "boots") ...]
    [(string=? s "other") ...]))

;; Template:
(define (shoe-temp sh)
  (cond
    [(shoe-brand sh) (... sh ...)]
    [(shoe-laced sh) (... sh ...)]
    [(shoe-kind sh) (shoekind-temp sh)]))

;; Part C

;; Consider the following function definitions:

;; foo : ShoeKind -> String
(define (foo u)
  (cond
    [(sneaker? u)
     (if (sneaker-is-running u)
         "running shoes"
         "sneakers")]
    [(string=? u "dress") "dress shoes"]
    [(string=? u "boots") "boots"]
    [(string=? u "other") ""]))                           

;; bar : Shoe -> String 
(define (bar t)
  (string-append (foo (shoe-kind t)) " by " (shoe-brand t)))

;; Write the signatures for the functions foo and bar.

;; Hint: The names "foo", "bar", "u", and "t" are not helpful. It may help you
;; to come up with better names as well. However, this is completely optional.
;; If you do so, feel free to change the names in the code above.

;; [TODO] Better signatures for foo and bar


